generate_rooted_tree <- function() {
  g <- igraph::graph.empty(directed = TRUE)
  g <- igraph::add_vertices(g, 2)
  g <- igraph::add_edges(g, c(1, 2))

  currentNodes <- c(2)
  nextNodeId <- 3

  for (level in 1:3) {
    newNodes <- vector("list", length(currentNodes) * 2)
    edgeList <- c()

    for (i in seq_along(currentNodes)) {
      node1 <- nextNodeId
      node2 <- nextNodeId + 1
      nextNodeId <- nextNodeId + 2
      newNodes[[i]] <- c(node1, node2)
      edgeList <- c(edgeList, c(currentNodes[i], node1), c(currentNodes[i], node2))
    }

    newNodeIds <- unlist(newNodes)
    g <- igraph::add_vertices(g, length(newNodeIds))
    g <- igraph::add_edges(g, edgeList)
    currentNodes <- newNodeIds
  }

  return(g)
}

plot_tree <- function(tree) {
  plot(tree, layout = layout.reingold.tilford(tree))
}

initializeBlockers <- function(params, tree) {
  N <- params$N
  num_nodes <- params$num_nodes
  root_node <- params$root_node
  blockedLayer <- params$blockedLayer
  numBlocked <- params$numBlocked
  propBlocked <- params$propBlocked

  blockedTraits <- matrix(0, nrow = N, ncol = num_nodes)

  blockedInds <- sample(1:N, round(N * propBlocked))
  trDistances <- igraph::distances(tree, v = root_node, mode = "out")

  blockableTraits <- which(trDistances == blockedLayer)
  if (length(blockableTraits) == 1) {
    blockedTraitIndices <- blockableTraits
  } else {
    blockedTraitIndices <- sample(blockableTraits, numBlocked)
  }
  blockedTraits[blockedInds, blockedTraitIndices] <- 1

  return(blockedTraits)
}

addDetours <- function(params, tree, blockedTraits, type = "serial") {
  num_nodes <- params$num_nodes
  root_node <- params$root_node
  blockedLayer <- params$blockedLayer
  numSteps <- params$numSteps
  
  if (type == "serial") {
    rootDistances <- igraph::distances(tree, v = root_node, mode = "out")
    for(blockedTrait in blockedTraits) {
      blockedDistances <- igraph::distances(tree, v = blockedTrait, mode = "out")
      preTraits <- igraph::neighbors(tree, blockedTrait, mode = "in")
      postTraits <- igraph::neighbors(tree, blockedTrait, mode = "out")
      for(preTrait in preTraits) {
        for(postTrait in postTraits) {
          maxNodeId <- max(igraph::V(tree))
          newNodes <- ((maxNodeId+1):(maxNodeId+numSteps))
          tree <- igraph::add_vertices(tree, numSteps)
          edgesToAdd <- c(preTrait, maxNodeId+1)
          if(numSteps > 1) {
            for (i in 1:(numSteps-1)) {
              edgesToAdd <- c(edgesToAdd, maxNodeId+i, maxNodeId+i+1)
            }
          }
          edgesToAdd <- c(edgesToAdd, maxNodeId+numSteps, postTrait)
          tree <- igraph::add_edges(tree, edgesToAdd)
        }
      }
    }
  } else if (type == "parallel") {
    rootDistances <- igraph::distances(tree, v = root_node, mode = "out")
    for (blockedTrait in blockedTraits) {
      blockedDistances <- igraph::distances(tree, v = blockedTrait, mode = "out")
      preTraits <- igraph::neighbors(tree, blockedTrait, mode = "in")
      postTraits <- igraph::neighbors(tree, blockedTrait, mode = "out")
      for (preTrait in preTraits) {
        for (postTrait in postTraits) {
          maxNodeId <- max(igraph::V(tree))
          newNodes <- ((maxNodeId + 1):(maxNodeId + numSteps))
          tree <- igraph::add_vertices(tree, numSteps)
          edgesToAdd <- c()
          for (newNode in newNodes) {
            edgesToAdd <- c(edgesToAdd, preTrait, newNode, newNode, postTrait)
          }
          tree <- igraph::add_edges(tree, edgesToAdd)
        }
      }
    }
  }
  return(tree)
}

initializePopulation <- function(params, blockers, tree) {
  N <- params$N
  num_nodes <- params$num_nodes
  root_node <- params$root_node
  numSteps <- params$numSteps
  numBlocked <- params$numBlocked
  repertoires <- matrix(0, nrow = N, ncol = num_nodes)
  repertoires[, root_node] <- 1

  regularTraits <- which(igraph::V(tree) <= num_nodes)
  trDistances <- igraph::distances(tree, v = root_node, to = setdiff(regularTraits, root_node))
  payoffs <- unlist(lapply(trDistances, function(x) 1 / x))
  payoffs <- c(payoffs, rep(1 / numSteps, numBlocked * numSteps))
  for (ind in 1:N) {
    blockedTraits <- which(blockers[ind, ] == 1)
    numTraits <- sample(1:(num_nodes - length(blockedTraits) - 1), 1)
    for (tr in 1:numTraits) {
      unknownTraits <- which(repertoires[ind, ] == 0)
      learnableTraits <- setdiff(unknownTraits, blockedTraits)
      if (length(learnableTraits) == 1) {
        repertoires[ind, learnableTraits] <- 1
      } else if (length(learnableTraits) > 1) {
        pList <- payoffs[which(1:ncol(repertoires) %in% learnableTraits)]
        chosenTrait <- sample(learnableTraits, 1, prob = pList)
        repertoires[ind, chosenTrait] <- 1
      }
    }
  }
  return(repertoires)
}


assignAges <- function(repertoires) {
  ## for age-based social learning, we need to assume initial ages.
  ## let's assume the age is proportional to the repertoire size
  ageScalar <- 1
  popAge <- rowSums(repertoires) * ageScalar
  return(popAge)
}

getDistances <- function(learnableTraits, knownTraits, tree) {
  ###
  # Get Distances between known traits and learnable traits, creating
  # duplicate columns if there are duplicates in learnable traits
  ###
  uniqueLearnableTraits <- unique(learnableTraits)
  distancesToUnique <- igraph::distances(tree, v = knownTraits, to = uniqueLearnableTraits)

  # Replicate distances based on the original 'learnableTraits' order and duplication
  trDistances <- distancesToUnique[, match(learnableTraits, uniqueLearnableTraits)]

  # Handle edge cases where 'trDistances' is a vector
  if (is.vector(trDistances)) {
    if (length(knownTraits) == 1) {
      trDistances <- matrix(trDistances, nrow = 1, byrow = TRUE)
    } else if (length(learnableTraits) == 1) {
      trDistances <- matrix(trDistances, ncol = 1)
    }
  }
  return(trDistances)
}

getTraitLearningProbability <- function(params, repertoires, ind, tree, learnableTraits, requirements) {
  if (length(learnableTraits) == 0) {
    return(numeric(0))
  }

  knownTraits <- which(repertoires[ind, ] == 1)
  pList <- rep(0, length(learnableTraits))
  for (i in 1:length(learnableTraits)) {
    targetTrait <- learnableTraits[i]
    if (length(requirements[targetTrait]) >= 2) {
      if (all(requirements[targetTrait][[1]] %in% knownTraits) | all(requirements[targetTrait][[2]] %in% knownTraits)) {
        pList[i] <- 1
      }
    } else if (length(requirements[targetTrait]) >= 1) {
      if (all(requirements[targetTrait][[1]] %in% knownTraits)) {
        pList[i] <- 1
      }
    }
  }

  if (length(pList) != length(learnableTraits)) {
    browser()
  }
  return(pList)
}

getPayoffs <- function(tree, params) {
  numBlocked <- params$numBlocked
  num_nodes <- params$num_nodes
  numSteps <- params$numSteps
  weight <- params$payoff_weight # Determines how random the effect of distance on a trait is
  root_node <- params$root_node
  payoff_scaling <- params$payoff_scaling # Determines how much distance affects payoff


  distances_from_root <- igraph::distances(tree, v = root_node, mode = "out")[1:num_nodes]
  distance_payoffs <- 1 + (distances_from_root - 1) * payoff_scaling

  random_payoffs <- runif(num_nodes)

  adjusted_payoffs <- (1 - weight) * (2 * random_payoffs / max(random_payoffs)) + weight * distance_payoffs
  adjusted_payoffs[params$root_node] <- 0
  adjusted_payoffs <- c(adjusted_payoffs, rep(1 / numSteps, numBlocked * numSteps)) # all auxiliary nodes get a partial payoff
  return(adjusted_payoffs)
}

trRequirements <- function(tree) {
  numTraits <- igraph::gorder(tree)

  # Initialize the list to store prerequisites for each trait
  requirements <- vector("list", numTraits)

  # Loop through each trait to find its prerequisites
  for (trait in 1:numTraits) {
    # Find direct predecessors (parents) of the trait
    predecessors <- igraph::neighbors(tree, trait, mode = "in")

    # Store the ids (or any other identifier) of required traits for the bonus
    requirements[[trait]] <- list(as.numeric(predecessors))
  }

  return(requirements)
}

augmentTrRequirements <- function(requirements, tree) {
  num_nodes <- length(requirements)
  unreachableTraits <- which(igraph::degree(tree, mode = "in") > 1)

  for (trait in unreachableTraits) {
    # Find parents that are auxiliary nodes
    parents <- igraph::neighbors(tree, trait, mode = "in")[which(igraph::neighbors(tree, trait, mode = "in") > num_nodes)]
    requirements[[trait]][[2]] <- c(parents)
  }
  return(requirements)
}


learnSocially <- function(params, repertoires, blockers, ind, learningStrategy, popAge, payoffs, tree, observedTraits, observedModels, prerequisites) {
  unknownTraits <- which(repertoires[ind, ] == 0)
  blockedTraits <- which(blockers[ind, ] == 1)
  learnableTraits <- observedTraits[which(observedTraits %in% unknownTraits & !observedTraits %in% blockedTraits)]
  if (length(observedTraits) > 0) {
    wList <- numeric(length = length(learnableTraits))
    pList <- getTraitLearningProbability(params, repertoires, ind, tree, learnableTraits, prerequisites)


    root_node <- params$root_node
    # Exit if the probability of learning any trait is zero
    if (sum(pList, na.rm = T) == 0 | all(learnableTraits == root_node)) {
      return(numeric(0))
    }

    ##### STRATEGY 1: payoff-based social learning #####
    if (learningStrategy == 1) {
      if (sum(payoffs[learnableTraits], na.rm = T) != 0) { # handle case where all payoffs are zero
        wList <- payoffs[learnableTraits] / sum(payoffs[learnableTraits])
      }
    }

    ###### STRATEGY 2: similarity based learning ######
    ## check for all agents how similar they are to self in skills
    else if (learningStrategy == 2) {
      usefulModels <- observedModels[observedTraits %in% learnableTraits]
      for (model in usefulModels) {
        modelIndex <- which(usefulModels == model)
        wList[modelIndex] <- sum(repertoires[ind, ] == repertoires[model, ]) / ncol(repertoires)
      }
    }

    ###### 	STRATEGY 3: age-based social learning #####
    ## check for all agents how similar they are to self in age
    else if (learningStrategy == 3) {
      usefulModels <- observedModels[observedTraits %in% learnableTraits]
      ageDif <- popAge[usefulModels] - popAge[ind]
      wList <- ifelse(ageDif >= 0, 0.5^ageDif, 10^-8)
    }

    ###### 	STRATEGY 4: conformist social learning #####
    ## Count the selected behaviours and weigh common ones more
    else if (learningStrategy == 4) {
      wList <- table(learnableTraits)[as.character(learnableTraits)]
    }

    ###### STRATEGY 0: random learning benchmark
    ## Randomly select a trait that is not yet learned
    else if (learningStrategy == 0) {
      wList <- rep(1, length(learnableTraits))
    }


    if (sum(wList, na.rm = T) == 0) {
      return(numeric(0))
    }

    if (any(is.na(wList * pList))) {
      print("NA in wList * pList")
      browser()
    }
    # browser if any is negative
    if (any(wList * pList < 0)) {
      print("Negative value in wList * pList")
      browser()
    }
    if (sum(pList) == 0) {
      print("Sum of pList is zero")
      browser()
    }
    if (length(wList) != length(pList)) {
      print("Length of wList and pList do not match")
      browser()
    }
    if (!any((wList * pList) > 0)) {
      print("no positive probabilities")
      browser()
    }

    ### MAKE CHOICE ###
    if (length(learnableTraits) == 1) {
      selectedTrait <- learnableTraits
      p <- pList
    } else {
      selectedTraitIndex <- sample(1:length(learnableTraits), 1, prob = wList * pList)
      selectedTrait <- observedTraits[selectedTraitIndex]
      p <- pList[selectedTraitIndex]
    }
    ## learn the trait with probability pList

    if (length(pList) > 0) {
      # if(runif(1) < p){
      repertoires[ind, selectedTrait] <- 1
      learnedTrait <- selectedTrait
      return(learnedTrait)
      # }
    }
  }
  return(numeric(0))
}
