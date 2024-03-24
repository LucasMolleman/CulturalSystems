## if showPopState==1, vary node size with portion of agents with that trait
plotTree <- function(params, tree, repertoires = NULL, showPopState = FALSE) {
  num_nodes <- params$num_nodes
  # Use layout_as_tree to create a tree layout
  layout <- layout_as_tree(tree, root=1, rootlevel=0)
  
  # Calculate the x-coordinates to center the tree horizontally
  levels <- distances(tree, v = 1, to = V(tree), mode = "out")
  level_widths <- table(levels)
  
  # Calculate the total width of the tree
  total_width <- max(level_widths)
  mid_x<-total_width/2
  x_coordinates<-c()
  for (level in level_widths) {
    x0<-mid_x
    for (k in 1:level){
      x<- (k-0.5) / level
      x_coordinates<-c(x_coordinates, x)
    }
  }
  # Set the x-coordinates in the layout
  layout[, 1] <- x_coordinates
  
  # set colouring according to depth
  nodeDepths<-1+distances(tree,v=1,to=V(tree),mode="out")
  colramp <- colorRampPalette(c("white", "blue","green","orange", "red"))
  color_palette <- colramp(max(nodeDepths))
  color <- color_palette[1:num_nodes]
  V(tree)$color <- color_palette[nodeDepths]
  
  ## plot the tree
  if (showPopState) {
    V(tree)$propAdopted <- rep(0,num_nodes)
    for (tr in 2:num_nodes) V(tree)$propAdopted[tr]<-mean(repertoires[,tr])
    plot(tree, layout = layout, vertex.size=30*V(tree)$propAdopted, edge.arrow.size = 0.5, edge.color='black',
         vertex.color = V(tree)$color, vertex.label=NA)	
  }
  else {
    plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color='black',
         vertex.color = V(tree)$color, vertex.label = V(tree)$name)
  }
}


# function to create a rooted tree which represents a cultural system
generate_rooted_tree_branching <- function(params, branching_factor) {
  num_nodes <- params$num_nodes
  
  g <- graph.empty(n = num_nodes, directed = TRUE)
  
  edgeList<-c()
  for (i in 1:(num_nodes-1)){
    #	outDegreeThisNode<-1+floor(runif(1)*branching_factor)
    #	outDegreeThisNode<-max(1, round(rnorm(1,mean=branching_factor,sd=1)))
    outDegreeThisNode<-branching_factor
    k<-i+1
    numConn<-0
    for (j in (i+1):num_nodes){
      if (numConn<outDegreeThisNode && !j%in%edgeList[c(FALSE,TRUE)]) {
        edgeList<-c(edgeList, i,j)
        numConn<-numConn+1
      }
    }
  }
  g<-add_edges(g, c(edgeList))
  return(g)
}


# Function to create a rooted tree which represents a cultural system
generate_rooted_tree_betaDistr <- function(params, tree_layers) {
  num_nodes <- params$num_nodes
  alpha1 <- params$alpha1
  beta1 <- params$beta1
  
  g <- graph.empty(n = num_nodes, directed = TRUE)
  edgeList<-c()
  
  ## define distribution of nodes across layers
  nodeLayer<-1 + rbbinom((num_nodes-1), (tree_layers-1), alpha = alpha1, beta = beta1)
  
  layerDistr<-rep(1,tree_layers)
  for (node in nodeLayer[1:(length(nodeLayer)-tree_layers)]){
    layerDistr[node]<-layerDistr[node]+1
  }
  
  layerNode<-0
  for (i in 1:length(layerDistr)) layerNode<-c(layerNode,rep(i,layerDistr[i]))
  inDegrees<-rep(0,num_nodes)
  
  for (layer in 0:(max(layerNode)-1)){
    if (layer==0) {
      for (j in which(layerNode==1)){
        edgeList<-c(edgeList,1,j)
        inDegrees[j]<-inDegrees[j]+1
      }
    }
    if (layer > 0){
      for (i in which(layerNode==layer)){
        nextLayer<-which(layerNode==(layer+1))	
        lowestInNextLayer<-min(inDegrees[nextLayer])
        
        candidates<-nextLayer[which(inDegrees[nextLayer]==lowestInNextLayer)]
        
        targetNodes<-candidates[1]
        if (length(targetNodes>0)){
          for (j in targetNodes){
            edgeList<-c(edgeList,i,j)
            inDegrees[j]<-inDegrees[j]+1
          }
        }
      }
      for (i in which(layerNode==(layer+1))){
        if (inDegrees[i] == 0){
          sourceNodes<-which(layerNode==layer)
          sourceNode<-ifelse (length(sourceNodes)==1, sourceNodes[1], sample(sourceNodes,1))
          edgeList<-c(edgeList,sourceNode,i)
          inDegrees[i]<-inDegrees[i]+1
        }
      }
    }
  }
  g<-add_edges(g, c(edgeList))
  return(g)
}


initializePopulation <- function(params){
  N <- params$N
  num_nodes <- params$num_nodes
  adj_matrix <- params$adj_matrix
  repertoires <- matrix(0, nrow = N, ncol = num_nodes)
  repertoires[, 1] <- 1
  
  for (ind in 1:N){
    numTraits <- sample(1:num_nodes, 1)
    for (tr in 2:numTraits){
      unknownTraits <- which(repertoires[ind, ] == 0)
      learnableTraits <- unknownTraits[sapply(unknownTraits, function(trait) prod(repertoires[ind, which(adj_matrix[, trait] == 1)] == 1)) == 1]
      if (length(learnableTraits) >= 1) {
        repertoires[ind, sample(learnableTraits, 1)] <- 1
      }
    }
  }
  return(repertoires)
}

assignAges<-function(repertoires){
  ## for age-based social learning, we need to assume initial ages. 
  ## let's assume the age is proportional to the repertoire size
  ageScalar<-1
  popAge<- rowSums(repertoires) * ageScalar
  return (popAge)
}

getLearnableTraits<-function(repertoires, ind, adj_matrix){
  # Identify traits not known by the individual
  unknownTraits <- which(repertoires[ind,] == 0)
  
  # Determine parents present in the individual's repertoire for each trait
  parentMatches <- colSums(adj_matrix == 1 & repertoires[ind,] == 1)
  
  # Identify traits that are learnable based on parent presence
  learnableTraits <- which(parentMatches > 0 & !is.na(parentMatches))
  
  # Filter learnable traits to include only those that are unknown
  learnableTraits <- learnableTraits[learnableTraits %in% unknownTraits]
  
  # Identify traits that cannot be learned due to negative relationships
  blockers <- which(colSums(adj_matrix == -1 & repertoires[ind,]) > 0)
  
  # Remove blocked traits from the list of learnable traits
  learnableTraits <- setdiff(learnableTraits, blockers)
  
  return(learnableTraits)
}

getDistances <- function(learnableTraits, knownTraits, tree) {
  ### 
  # Get Distances between known traits and learnable traits, creating
  # duplicate columns if there are duplicates in learnable traits 
  ###
  uniqueLearnableTraits <- unique(learnableTraits)
  distancesToUnique <- distances(tree, v = knownTraits, to = uniqueLearnableTraits)
  
  # Replicate distances based on the original 'learnableTraits' order and duplication
  replicatedDistances <- distancesToUnique[,match(learnableTraits, uniqueLearnableTraits)]
  
  return(replicatedDistances)
}


getTraitLearningProbability <- function(repertoires, ind, tree, learnableTraits, falloffFunction = "adjacent"){
  if(length(learnableTraits) == 0){
    return(numeric(0))
  }
  
  knownTraits <- which(repertoires[ind,] == 1)
  
  trDistances <- getDistances(learnableTraits, knownTraits, tree)  
  
  
  if(is.vector(trDistances)) {
    if(length(knownTraits) == 1) {
      trDistances <- matrix(trDistances, nrow = 1, byrow = TRUE)
    } else if (length(learnableTraits) == 1) {
      trDistances <- matrix(trDistances, ncol = 1)
    }
  }
  # Handle different falloff functions
  if (falloffFunction == "adjacent") {
    pList <- apply(trDistances, MARGIN = 2, FUN = function(x) if(min(x) == 1) 1 else 0)
  }
  else if (falloffFunction == "reciprocal"){
    pList <- apply(trDistances, MARGIN = 2, FUN = function(x) sum(1/x^2))
  }
  else if (falloffFunction == "linear") {
    maxDistance <- max(trDistances)
    pList <- apply(trDistances, MARGIN = 2, FUN = function(x) max(0, 1 - min(x) / maxDistance))
  }
  if(length(pList)!= length(learnableTraits)){
    browser()
  }
  return(pList)
}
  
getPayoffs <- function(tree, params) {
  strength <- params$payoff_scaling
  distances_from_root <- distances(tree, v = 1, mode = "out")
  max_distance <- max(distances_from_root)
  payoffs <- runif(vcount(tree))
  
  adjusted_payoffs <- (1 - strength) * (2 * payoffs / max(payoffs)) + 
    strength * (distances_from_root / max_distance)
  
  adjusted_payoffs
}

learnSocially <- function(repertoires, ind, adj_matrix, learningStrategy,  popAge,  payoffs, tree, observedTraits, observedModels, falloffFunction){
  unknownTraits <- which(repertoires[ind,] == 0)
  learnableTraits <- observedTraits[which(observedTraits %in% unknownTraits)]

  if(length(observedTraits) > 0){
    wList <- numeric(length = length(learnableTraits))     
    pList <- getTraitLearningProbability(repertoires, ind, tree, learnableTraits, falloffFunction = falloffFunction) 
    
    # Exit if the probability of learning any trait is zero
    if(sum(pList) == 0){
      return(numeric(0))
    }

    ##### STRATEGY 1: payoff-based social learning #####
    if(learningStrategy == 1){
      wList <- payoffs[learnableTraits] / sum(payoffs[learnableTraits])
    }
    
    ###### STRATEGY 2: similarity based learning ######
    ## check for all agents how similar they are to self in skills
    else if(learningStrategy == 2){
      usefulModels <- observedModels[observedTraits %in% unknownTraits]
      for(model in usefulModels){
        modelIndex <- which(usefulModels == model)
        wList[modelIndex] <- sum(repertoires[ind,] == repertoires[model,])/ncol(repertoires)
      }
    }
    
    ######	STRATEGY 3: age-based social learning #####
    ## check for all agents how similar they are to self in age
    else if(learningStrategy == 3){
      usefulModels <- observedModels[observedTraits %in% unknownTraits]
      ageDif <- popAge[usefulModels] - popAge[ind]
      wList <- ifelse(ageDif >= 0, 0.5 ^ ageDif, 10 ^ -8)
    }
    
    ######	STRATEGY 4: conformist social learning #####
    ## Count the selected behaviours and weigh common ones more
    else if(learningStrategy == 4){
      wList <- table(learnableTraits)[as.character(learnableTraits)]
    }
    
    ###### STRATEGY 0: random learning benchmark
    ## Randomly select a trait that is not yet learned
    else if(learningStrategy == 0){
      wList <- rep(1, length(learnableTraits))
    }

    ## Temporary fix so that traits with 0 probability are not considered
    # browser()
    # wList <- wList[pList > 0]
    # learnableTraits <- learnableTraits[pList > 0]
    # pList <- pList[pList > 0]
    pList <- pmin(pList, 1)
    
    
    if(sum(pList) == 0) browser()
    ### MAKE CHOICE ###
    selectedTraitIndex <- sample(1:length(learnableTraits), 1, prob = wList * pList)
    selectedTrait <- observedTraits[selectedTraitIndex]
    p <- pList[selectedTraitIndex]
    
    ## learn the trait with probability pList

    if(length(pList) > 0){
      if(runif(1) < p){
        repertoires[ind, selectedTrait] <- 1
        learnedTrait <- selectedTrait
        return(learnedTrait)
      }
    } 
  }
  return(numeric(0))
}


