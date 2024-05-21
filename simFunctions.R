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
  blockedTraits
}

bookkeep_traits <- function(repertoires, blockedInds){
  browser()
  num_traits <- ncol(repertoires)
  prop_adopted <- rep(NA, num_traits - 1)
  for (trait in 2:num_traits) {
    prop_adopted[trait] <- mean(repertoires[blockedInds, trait])
  }
  prop_adopted
}

addDetours <- function(params, tree, blockedTraits, type = "serial") {
  requirements <- attributes(tree)$requirements
  payoffs <- attributes(tree)$payoffs
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
  attributes(tree)$requirements <- requirements
  attributes(tree)$payoffs <- payoffs
  return(tree)
}

sample_initial_traits <- function(ind, repertoire, blockedTraits, numTraits, payoffs){
  for(trait in seq_along(numTraits)){
    unknownTraits <- which(repertoire == 0)
    learnableTraits <- setdiff(unknownTraits, blockedTraits)
    if (length(learnableTraits) == 1) {
      repertoire[learnableTraits] <- 1
      return(repertoire)
    } 
    if (length(learnableTraits) > 1) {
      pList <- payoffs[which(1:length(repertoire) %in% learnableTraits)]
      chosenTrait <- sample(learnableTraits, 1, prob = pList)
      repertoire[chosenTrait] <- 1
      return(repertoire)
    }
  }
}

initializePopulation <- function(params, blockers, tree) {
  N <- params$N
  root_node <- params$root_node
  num_nodes <- igraph::gorder(tree)
  repertoires <- matrix(0, nrow = N, ncol = num_nodes)
  repertoires[, root_node] <- 1
  
  regularTraits <- which(igraph::V(tree) <= num_nodes)
  payoffs <- attributes(tree)$payoffs
  for (ind in 1:N) {
    blockedTraits <- which(blockers[ind, ] == 1)
    numTraits <- sample(1:(num_nodes - length(blockedTraits) - 1), 1)
    repertoires[ind, ] <- sample_initial_traits(ind, repertoires[ind, ], blockedTraits, numTraits, payoffs)
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



getEnvironmentalLearnability <- function(params, inds, repertoires, adj_matrix, tree, blockers){
  requirements <- attributes(tree)$requirements
  p <- c()
  
  for(ind in inds){
    
    knownTraits <- which(repertoires[ind,] == 1)
    
    unknownTraits <- which(repertoires[ind,] == 0)
    
    if(length(unknownTraits) == 0){
      p[ind] <- NA 
      next
    }
    blockedTraits <- which(blockers[ind,] == 1)
    learnableTraits <- unknownTraits[which(!unknownTraits %in% blockedTraits)]
    
    pList <- RcppFunctions::getTraitLearningProbability(repertoires, ind, attributes(tree)$requirements, learnableTraits)
    
    learnableTraits <- learnableTraits[which(pList == 1)]
    
    freqLearnable <- 0
    
    popOthers <- repertoires[-ind,]
    
    
    for(trait in learnableTraits){
      freqLearnable <- freqLearnable + sum(popOthers[,trait])
    }
    
    freqUnknown <- 0
    
    for(trait in unknownTraits){
      freqUnknown <- freqUnknown + sum(popOthers[,trait])
    }
    
    p[ind] <- freqLearnable / freqUnknown
  }
  
  return(mean(p, na.rm=T))
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
  adjusted_payoffs <- c(adjusted_payoffs, rep(1 / numSteps, numBlocked * numSteps * 2)) # all auxiliary nodes get a partial payoff
  print(adjusted_payoffs)
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

augmentTrRequirements <- function(tree) {
  requirements <- attr(tree, "requirements")
  num_nodes <- length(requirements)
  unreachableTraits <- which(igraph::degree(tree, mode = "in") > 1)

  for (trait in unreachableTraits) {
    # Find parents that are auxiliary nodes
    parents <- igraph::neighbors(tree, trait, mode = "in")[which(igraph::neighbors(tree, trait, mode = "in") > num_nodes)]
    requirements[[trait]][[2]] <- c(parents)
  }
  return(requirements)
}


learnSocially <- function(params, repertoires, blockers, ind, learningStrategy, popAge, tree, observedTraits, observedModels) {
  payoffs <- attributes(tree)$payoffs
  unknownTraits <- which(repertoires[ind, ] == 0)
  blockedTraits <- which(blockers[ind, ] == 1)
  learnableTraits <- observedTraits[which(observedTraits %in% unknownTraits & !observedTraits %in% blockedTraits)]
  if (length(observedTraits) > 0) {
    wList <- numeric(length = length(learnableTraits))
    pList <- RcppFunctions::getTraitLearningProbability(repertoires, ind, attributes(tree)$requirements, learnableTraits)

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
    
    else if (learningStrategy == 5) {
      usefulModels <- observedModels[observedTraits %in% learnableTraits]
      blockedInds <- which(rowSums(blockers) > 0)
      #if blocked, set weight of all models that are also blocked to 1, others to 0
      if (ind %in% blockedInds) {
        if (any(usefulModels %in% blockedInds)) {
          for (model in usefulModels) {
            modelIndex <- which(usefulModels == model)
            wList[modelIndex] <- ifelse(model %in% blockedInds, 1, 0)
          }
        } else { #else, regular similarity-based learning
          for (model in usefulModels) {
            modelIndex <- which(usefulModels == model)
            wList[modelIndex] <- sum(repertoires[ind, ] == repertoires[model, ]) / ncol(repertoires)
          }
        }
     } else { #if not blocked, regular similarity-based learning
        for (model in usefulModels) {
          modelIndex <- which(usefulModels == model)
          wList[modelIndex] <- sum(repertoires[ind, ] == repertoires[model, ]) / ncol(repertoires)
        }
      }
    }
    
    else if (learningStrategy == 6) {
      # prefer individuals with learning difficulties. If none are available, choose randomly
      usefulModels <- observedModels[observedTraits %in% learnableTraits]
      blockedInds <- which(rowSums(blockers) > 0)
      for (model in usefulModels) {
        modelIndex <- which(usefulModels == model)
        wList[modelIndex] <- ifelse(model %in% blockedInds, 1, sum(repertoires[ind, ] == repertoires[model, ]) / ncol(repertoires))
      }
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
      wList <- rep(1, length(wList))
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


# Not used
getTraitLearningProbability_R <- function(repertoires, ind, tree, learnableTraits) {
  requirements <- attributes(tree)$requirements
  
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
