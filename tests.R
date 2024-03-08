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
      wList2 <- as.numeric(table(factor(observedBehaviours, levels = unique(observedBehaviours))))
      wList <- table(observedBehaviours)[as.character(observedBehaviours)]
    }
    
    ###### STRATEGY 0: random learning benchmark
    ## Randomly select a trait that is not yet learned
    else if(learningStrategy == 0){
      wList <- rep(1, length(observedBehaviours))
    }
    
    ### MAKE CHOICE ###
    selectedTrait<-ifelse(length(observedBehaviours)==1, observedBehaviours[1], sample(observedBehaviours,1,prob=wList))
    return(selectedTrait)	
  }
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
  num_nodes = 16,
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
  numTraits <- sample(2:10, 1)  # Decides on a random target trait count between 2 and 10
  
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
  ind <- 71
  
  r <- runif(1)
  
  learnableTraits <- getLearnableTraits(repertoires, ind, adj_matrix)
  if (length(learnableTraits) > 0) {
    if (r < params$S) {
      set.seed(1)
      result1 <- learnSocially(params, repertoires, ind, adj_matrix, learningStrategy, popAge, payoffs)
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