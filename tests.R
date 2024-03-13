library(igraph)

learnSocially <- function(repertoires, ind, adj_matrix, learningStrategy,  popAge,  payoffs, tree, observedBehaviours, observedModels){
  if(length(observedBehaviours) > 0){
    wList <- numeric(length = length(observedBehaviours))
    pList <- getTraitLearningProbability(repertoires, ind, tree, observedBehaviours)
    
    if(length(pList) == 0){
      return(numeric(0))
    }
    
    
    ##### STRATEGY 1: payoff-based social learning #####
    if(learningStrategy == 1){
      wList <- payoffs[observedBehaviours] / sum(payoffs[observedBehaviours])
    }
    
    ###### STRATEGY 2: similarity based learning ######
    ## check for all agents how similar they are to self in skills
    else if(learningStrategy == 2){
      wList <- sapply(observedModels, function(model) {
        sum(repertoires[model,] == repertoires[ind,])
      })
    }
    
    ######	STRATEGY 3: age-based social learning #####
    ## check for all agents how similar they are to self in age
    else if(learningStrategy == 3){
      ageDif <- popAge[observedModels] - popAge[ind]
      wList <- ifelse(ageDif >= 0, 0.5 ^ ageDif, 10 ^ -8)
    }
    
    ######	STRATEGY 4: conformist social learning #####
    ## Count the selected behaviours and weigh common ones more
    else if(learningStrategy == 4){
      wList <- table(observedBehaviours)[as.character(observedBehaviours)]
    }
    
    ###### STRATEGY 0: random learning benchmark
    ## Randomly select a trait that is not yet learned
    else if(learningStrategy == 0){
      wList <- rep(1, length(observedBehaviours))
    }
    
    ### MAKE CHOICE ###
    
    selectedTraitIndex <- sample(1:length(observedBehaviours), 1, prob = wList)
    selectedTrait <- observedBehaviours[selectedTraitIndex]
    pList <- pList[selectedTraitIndex]
    
    ## learn the trait with probability pList
    
    if(runif(1) < pList){
      repertoires[ind, selectedTrait] <- 1
      learnedTrait <- selectedTrait
      return(learnedTrait)
    } 
  }
  return(numeric(0))
}



learnSocially_old <- function(repertoires, ind, adj_matrix, learningStrategy, M, N, popAge){
  ## sample M random other individuals
  poolOthers<-setdiff(1:nrow(repertoires), ind) # agents do not sample themselves
  models<-sample(poolOthers, M, replace=FALSE)
  
  
  ## randomly pick 1 trait from each model
  ## only consider traits the learning agent do not know yet
  observedBehaviours<-c()
  observedModels<-c()
  for (model in models){
    newTraits<-which(repertoires[model,] == 1 & repertoires[ind,] == 0)
    if(length(newTraits)>0){
      tr<-sample(newTraits,1)
      observedBehaviours<-c(observedBehaviours, tr)
      observedModels<-c(observedModels, model)
    }
  }
  
  ## if there's no trait to learn among the observed ones, skip
  ## SLpay was initialized at NA so nothing happens in that case
  ## if there IS something to learn:
  if (length(observedBehaviours)>0){
    ## list of weights of each observed behaviour
    ## weights depend on learning strategy
    ## normalized weights are used for choice
    wList<-c()
    selectedTrait<-c()	
    ##### STRATEGY 1: payoff-based social learning #####
    
    if (learningStrategy==1){
      wList<-payoffs[observedBehaviours]/sum(payoffs[observedBehaviours])
    }
    ##################
    
    ###### STRATEGY 2: similarity based learning ######
    if (learningStrategy==2){
      ## check for all agents how similar they are to self
      for (mod in observedModels){
        simToFocal<-sum(repertoires[ind,] == repertoires[mod,])
        wList<-c(wList, simToFocal)
      }
    }
    ##################	
    ######	STRATEGY 3: age-based social learning #####				
    if (learningStrategy==3){
      ## based on age similarity
      ## check for all agents how similar they are to self
      for (mod in observedModels){
        w<- 10^-8
        ageDif<-popAge[mod]-popAge[ind]								
        if (ageDif >=0) w<- 0.5^ageDif
        wList<-c(wList, w)
      }			
    }	
    ################
    
    ######	STRATEGY 4: conformist social learning #####				
    if (learningStrategy==4){
      ## count the selected behaviours and weigh common ones more
      for (mod in 1:length(observedBehaviours)){
        w<- length(which(observedBehaviours==observedBehaviours[mod]))
        wList<-c(wList, w)
      }			
    }
    
    ################
    
    
    ###### STRATEGY 0:  random learning benchmark
    if (learningStrategy==0){
      ## select a trait you dont have at random
      wList<-rep(1,length(observedBehaviours))							
    }
    ############
    
    ### MAKE CHOICE ###
    selectedTrait<-ifelse(length(observedBehaviours)==1, observedBehaviours[1], sample(observedBehaviours,1,prob=wList))
    return(selectedTrait)	
  }						
}

# Test if the new learnSocially function behaves identically to the old one

set.seed(123)

params <- list(
  num_nodes = 64,
  N = 100,
  M = 10,
  timesteps = 5000,
  replicateSimulations = 30,
  S = 0.99,
  reset_rate = 0.01,
  alpha1 = 1,
  beta1 = 1,
  olderPref = 0,
  windowSize = 1000
)

branching_factor <- 1
tree <- generate_rooted_tree_branching(params, branching_factor)

adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
adj_matrix[1, 1] <- 1

repertoires <- matrix(0, nrow = params$N, ncol = params$num_nodes)
for (ind in 1:params$N) {
  currentTraits <- c(1)
  numTraits <- sample(2:12, 1)  # Decides on a random target trait count between 2 and 12 for initializing the test repertoire
  
  while (length(currentTraits) < numTraits) {
    # Find traits directly adjacent to the most recently added trait
    lastTrait = tail(currentTraits, n=1)
    # Using adj_matrix to identify possible direct adjacents of the lastTrait
    # These adjacents are not previously included in currentTraits
    possibleAdjacents <- which(adj_matrix[lastTrait,] == 1)
    # Filtering for unacquired traits
    adjacentNewTraits <- setdiff(possibleAdjacents, currentTraits)
    
    if (length(adjacentNewTraits) > 0) {
      # Randomly add one of the directly adjacent, unacquired new traits
      newTrait <- sample(adjacentNewTraits, 1)
      currentTraits <- c(currentTraits, newTrait)
    } else {
      # No directly adjacent new trait to add; break the loop
      break
    }
  }
  
  repertoires[ind, currentTraits] <- 1
}


popAge <- assignAges(repertoires)

payoffs <- runif(params$num_nodes)
payoffs <- 2 * payoffs / max(payoffs)

compareFunctions <- function(learningStrategy) {
  ind <- 76
  
  r <- runif(1)
  
  learnableTraits <- getLearnableTraits(repertoires, ind, adj_matrix)
  if (length(learnableTraits) > 0) {
    if (r < params$S) {
      ## sample M random other individuals
      poolOthers <- setdiff(1:nrow(repertoires), ind) # agents do not sample themselves
      models<-sample(poolOthers, params$M, replace=FALSE)
      
      ## randomly pick 1 trait from each model
      ## only consider traits the learning agent do not know yet
      observedBehaviours<-c()
      observedModels<-c()
      for (model in models){
        newTraits<-which(repertoires[model,] == 1 & repertoires[ind,] == 0)
        if(length(newTraits)>0){
          tr<-sample(newTraits,1)
          observedBehaviours<-c(observedBehaviours, tr)
          observedModels<-c(observedModels, model)
        }
      }
      
      
      
      set.seed(1)
      result1 <- learnSocially(repertoires, ind, adj_matrix, learningStrategy, popAge, payoffs, observedBehaviours, observedModels)
      set.seed(1)
      result2 <- learnSocially_old(repertoires, ind, adj_matrix, learningStrategy, params$M, params$N, popAge)
      
      if (identical(result1, result2)) {
        cat("Learning Strategy", learningStrategy, "- The functions behave identically.\n")
      } else {
        cat("Learning Strategy", learningStrategy, "- The functions behave differently.\n", "result1:", result1, "result2:", result2)
      }
    } else {
      selectedTrait <- learnableTraits[1]
      if (length(learnableTraits) > 1) {
        selectedTrait <- sample(learnableTraits, 1)
      }
      repertoires[ind, selectedTrait] <- 1
    }
  }
}



for (learningStrategy in 0:4) {
  compareFunctions(learningStrategy)
}










## sample M random other individuals
poolOthers <- setdiff(1:nrow(repertoires), ind) # agents do not sample themselves
models<-sample(poolOthers, params$M, replace=FALSE)

## randomly pick 1 trait from each model
## only consider traits the learning agent do not know yet
observedBehaviours<-c()
observedModels<-c()
for (model in models){
  newTraits<-which(repertoires[model,] == 1 & repertoires[ind,] == 0)
  if(length(newTraits)>0){
    print(newTraits)
    tr<-sample(newTraits,1)
    observedBehaviours<-c(observedBehaviours, tr)
    observedModels<-c(observedModels, model)
  }
}
result1 <- learnSocially(repertoires, 76, adj_matrix, 1, popAge, payoffs, tree, observedBehaviours, observedModels)


nodeDepths<-1+distances(tree,v=1,to=V(tree),mode="out")
maxNodeDepth<-max(nodeDepths)
## bookkeep the frequency of traits across tree depths (for checking stability over time)
traitsAtDepth<-matrix(NA, nrow=params$timesteps, ncol=max(nodeDepths))




repl <- 1

runsimulation(params, learningStrategy, repl, tree)

params$num_nodes <- 8
branching_factor <- 3
tree <- generate_rooted_tree_branching(params, branching_factor)
plotTree(params, tree, repertoires)

g <- tree
plot(g)
v_count <- vcount(g)
all_pairs <- expand.grid(from = 1:v_count, to = 1:v_count)
all_pairs <- subset(all_pairs, from != to)
weights <- mapply(function(from, to) {
  distance <- distances(g, v = from, to = to)
  if(distance > 0) 1 / distance^2 else 0
}, from = all_pairs$from, to = all_pairs$to)
all_pairs$weight <- weights
g_full <- graph_from_data_frame(all_pairs, directed = FALSE)
plot(g_full, edge.width = E(g_full)$weight * 10)

plotTree <- function(params, tree) {
  num_nodes <- params$num_nodes
  
  layout <- layout_as_tree(tree, root = 1, rootlevel = 0)
  
  levels <- distances(tree, v = 1, to = V(tree), mode = "out")
  level_widths <- table(levels)
  
  total_width <- max(level_widths)
  mid_x <- total_width / 2
  x_coordinates <- numeric(length = num_nodes)
  used <- numeric(length = max(levels) + 1)
  
  for(i in seq_along(levels)) {
    level <- levels[i] + 1
    x_coordinates[i] <- (used[level] + 0.5) * (total_width / level_widths[as.character(level - 1)]) - mid_x
    used[level] <- used[level] + 1
  }
  
  layout[, 1] <- x_coordinates
  
  nodeDepths <- 1 + distances(tree, v = 1, to = V(tree), mode = "out")
  colramp <- colorRampPalette(c("white", "blue", "green", "orange", "red"))
  color_palette <- colramp(max(nodeDepths))
  
  plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color = 'black',
       vertex.color = color_palette[nodeDepths], vertex.label = NA)
}

params <- list(num_nodes = 10)
branching_factor = 2
g <- generate_rooted_tree_branching(params, branching_factor)
plotTree(params, g)


generate_full_connection_with_weights <- function(tree) {
  v_count <- vcount(tree)
  all_pairs <- expand.grid(from = 1:v_count, to = 1:v_count)
  all_pairs <- subset(all_pairs, from != to)
  weights <- mapply(function(from, to) {
    distance <- distances(tree, v = from, to = to)
    if(distance > 0) return(1 / distance^2) else return(0)
  }, from = all_pairs$from, to = all_pairs$to)
  all_pairs$weight <- weights
  
  tree_full <- graph_from_data_frame(all_pairs, directed = FALSE)
  E(tree_full)$weight <- all_pairs$weight
  return(tree_full)
}

library(igraph)
library(dplyr)

plotFullTreeUniqueConnections <- function(original_tree) {
  # Assuming 'original_tree' is already provided and correctly defined outside this function.
  
  # Generate positions using the tree layout
  layout <- layout_as_tree(original_tree)
  
  # Calculate distances within the original tree for direct connections
  distances_matrix <- distances(original_tree)
  
  # Generate a graph representing full connections between nodes
  g_full <- graph.full(vcount(original_tree))
  
  # Prepare edge attributes
  E(g_full)$weight <- mapply(function(from, to) { 
    if(distances_matrix[from, to] == 1) { # Direct edge
      return(1) 
    } else { # Indirect edge
      distance <- distances_matrix[from, to]
      return(1 / distance^2) 
    }
  }, from = ends(g_full, E(g_full))[,1], to = ends(g_full, E(g_full))[,2])
  
  edge_colors <- ifelse(E(g_full)$weight == 1, "black", "grey")
  edge_widths <- ifelse(E(g_full)$weight == 1, 2, 1)
  edge_labels <- ifelse(E(g_full)$weight == 1, "1", sprintf("%.2f", E(g_full)$weight))
  
  # Set vertex color according to the depth
  node_depths <- distances(original_tree, v = 1, to = V(original_tree), mode = "out")
  colramp <- colorRampPalette(c("white", "blue", "green", "orange", "red"))
  color_palette <- colramp(max(node_depths))
  vertex_colors <- color_palette[as.numeric(node_depths) + 1]
  
  # Plot
  plot(g_full, layout = layout, edge.label = edge_labels, edge.label.color = "red", edge.label.cex = 0.6,
       edge.arrow.size = 0, edge.color = edge_colors, edge.width = edge_widths,
       vertex.color = vertex_colors, vertex.label = NA, 
       main = "Full Tree with Direct and Weighted Indirect Connections")
  
  # Highlight direct edges by overplotting to ensure their labels ("1") are placed correctly
  direct_edges <- which(E(g_full)$weight == 1)
  with(E(g_full)[direct_edges], {
    plot(g_full, layout = layout, edge.label = "1", edge.label.color = "black", edge.label.cex = 0.6,
         edge.arrow.size = 0, edge.color = "black", edge.width = 2,
         vertex.color = vertex_colors, vertex.label = NA, add = TRUE)
  })
}

# Parameters and original_tree definition assumed to be done as before
plotFullTreeUniqueConnections(original_tree)


