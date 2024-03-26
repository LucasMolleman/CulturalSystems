combineResults <- function(accum, new) {
  if (is.null(accum)) { # Check if accum is null (for the first combination)
    return(new) # Simply return new if accum has not been initialized
  } else {
    #logic to combine accumulated and new results
    accum <- rbind(accum, new)
    return(accum)
  }
}

runsimulation <- function(params, learningStrategy, repl, tree){  
  ### define the cultural system ###
  
  
  ## derive square matrix of parent/child traits
  adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
  ## root trait (at position 1,1) is its own parent
  adj_matrix[params$root_node,params$root_node]<-1 
  
  ## bookkeeping for output
  SLpay<-rep(NA,params$timesteps)				## payoff for social learning
  
  
  # 		set payoffs for each trait
  
  payoffs <- getPayoffs(tree, params)
  ### SYSTEM AND NODE PAYOFFS ARE SET
  
  ####### INITIALIZE POPULATION #####
  repertoires<-initializePopulation(params)
  popAge<-assignAges(repertoires)
  
  ### population is now initialized... start running the model
  for (t in 1:params$timesteps){
    ## sample a random individual
    ind<-sample(1:params$N,1)
    
    ## will they learn individually or socially?
    r<-runif(1)
    unknownTraits <- which(repertoires[ind,] == 0) 
    SLpay[t]<-NA
    if (length(unknownTraits > 0)){  #only try to learn if there's anything to learn for this agent
      
      if (r<params$S) {  # social learning
        ## sample M random other individuals
        poolOthers <- setdiff(1:nrow(repertoires), ind) # agents do not sample themselves
        models<-sample(poolOthers, params$M, replace=FALSE)
        
        ## randomly pick 1 trait from each model
        ## only consider traits the learning agent do not know yet
        observedTraits<-c()
        observedModels<-c()
        for (model in models){
          newTraits<-which(repertoires[model,] == 1 & repertoires[ind,] == 0)
          if(length(newTraits)>0){
            tr<-sample(newTraits,1)
            observedTraits<-c(observedTraits, tr)
            observedModels<-c(observedModels, model)
          }
        }

        learnedTrait <- learnSocially(repertoires,
                                      ind,
                                      adj_matrix,
                                      learningStrategy, 
                                      popAge,
                                      payoffs,
                                      tree,
                                      observedTraits,
                                      observedModels,
                                      params$falloffFunction)														
        
        if (length(learnedTrait)==1){
        ######## calculate payoffs of learning
          repertoires[ind, learnedTrait] <- 1
          focalPay <- payoffs[learnedTrait]
          SLpay[t]<- focalPay
        }
        else{
          SLpay[t] <- 0
        }
	
      }
      else if(r >= params$S){	# individual learning (=innovation)
        selectedTrait <- sample(unknownTraits,1)
        # Calculate learning probability based on distance
        pList <- unique(getTraitLearningProbability(repertoires, ind, tree, selectedTrait))
        if(length(pList) > 0){
          if (runif(1) < pList[1]){
            repertoires[ind,selectedTrait]<-1
          }
        }
      }							
    }
    ## each time step the agent was sampled, their age increases by 1
    popAge[ind]<-popAge[ind]+1
    
    ## replace an individual with a naive one at random
    ## NB this is not appropriate for evolutionary sims
    if (runif(1) < params$reset_rate) {
      repertoires[ind,]<-c(1,rep(0,params$num_nodes-1))
      popAge[ind]<-0  ## reset the age of the agent to 0
    }
  }
  sumThisSimulation<-c(params$num_nodes, 
                        branching_factor, 
                        params$tree_layers, 
                        params$alpha1,
                        params$beta1,
                        learningStrategy,
                        params$olderPref,
                        repl,
                        params$payoff_scaling,
                        mean(SLpay[round(params$timesteps*0.9):params$timesteps], na.rm=TRUE))

  
  return(sumThisSimulation)
}
