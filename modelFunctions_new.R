## if showPopState==1, vary node size with portion of agents with that trait
plotTree <- function(params, tree, repertoires, showPopState = FALSE) {
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
         vertex.color = V(tree)$color, vertex.label=NA)
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


initializePopulation<-function(params){
  ## start with empty repertoires (but fill them up in the next step)
  N <- params$N
  num_nodes <- params$num_nodes
  adj_matrix <- params$adj_matrix
  
  repertoires <- matrix(0, nrow = N, ncol = num_nodes)
  
  ## everyone has the root trait of the cultural system
  repertoires[, 1] <- 1
  
  ### initialize the population with repertoires (all connected to the tree root) 
  ### with sizes drawn from a uniform distribution between 1:num_nodes
  for (ind in 1:N){
    numTraits <- sample(1:num_nodes, 1)               # number of traits of this agent
    traitsToAdd <- sample(2:num_nodes, numTraits - 1) # add (randomly chosen) learnable traits
    repertoires[ind, traitsToAdd] <- 1
    # Ensure that only learnable traits based on the tree structure are added
    for (trait in traitsToAdd){
      if (!any(adj_matrix[trait, ] == 1 & repertoires[ind, ] == 1)){
        repertoires[ind, trait] <- 0
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




learnSocially <- function(params, repertoires, ind, adj_matrix, learningStrategy,  popAge,  payoffs){
  M <- params$M
  olderPref <- params$olderPref
  ## sample M random other individuals
  poolOthers <- setdiff(1:nrow(repertoires), ind) # agents do not sample themselves
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
  
  
  if(length(observedBehaviours) > 0){
    wList <- numeric(length = length(observedBehaviours))
    
    ##### STRATEGY 1: payoff-based social learning #####
    if(learningStrategy == 1){
      wList <- payoffs[observedBehaviours] / sum(payoffs[observedBehaviours])
    }
    
    ###### STRATEGY 2: similarity based learning ######
    ## check for all agents how similar they are to self in skills
    else if(learningStrategy == 2){
      wList <- sapply(observedModels, function(mod) sum(repertoires[ind, ] == repertoires[mod, ]))
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
      w <- table(observedBehaviours)[as.character(observedBehaviours)]
      wList <- as.numeric(w)
    }
    
    ###### STRATEGY 0: random learning benchmark
    ## Randomly select a trait that is not yet learned
    else if(learningStrategy == 0){
      wList <- rep(1, length(observedBehaviours))
    }
    
    ### MAKE CHOICE ###
    selectedTrait<-ifelse(length(observedBehaviours)==1, observedBehaviours[1], sample(observedBehaviours,1,prob=wList))
    #		if (length(observedBehaviours)==1) {selectedTrait<-observedBehaviours[1]}
    #		else {selectedTrait<-sample(observedBehaviours, 1, prob=wList)}
    return(selectedTrait)	
  }
}	

combineResults <- function(accum, new) {
  if (is.null(accum)) { # Check if accum is null (for the first combination)
    return(new) # Simply return new if accum has not been initialized
  } else {
    # Your logic to combine accum and new
    accum$strategySuccess <- rbind(accum$strategySuccess, new$strategySuccess)
    accum$summSLpay <- rbind(accum$summSLpay, new$summSLpay)
    accum$meanTraitsInSystem <- c(accum$meanTraitsInSystem, new$meanTraitsInSystem)
    
    return(accum)
  }
}
