combineResults <- function(accum, new) {
  if (is.null(accum)) { # Check if accum is null (for the first combination)
    return(new) # Simply return new if accum has not been initialized
  } else {
    #logic to combine accumulated and new results
    accum <- rbind(accum, new)
    return(accum)
  }
}

runsimulation <- function(params, learningStrategy, repl, tree, nodeDepths, maxNodeDepth, traitsAtDepth){  
  
  ### define the cultural system ###
  
  
  ## derive square matrix of parent/child traits
  adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
  ## root trait (at position 1,1) is its own parent
  adj_matrix[1,1]<-1 
  
  ## bookkeeping for output
  SLpay<-rep(NA,params$timesteps)				## payoff for social learning
  
  
  # 		set payoffs for each trait
  
  payoffs<-runif(params$num_nodes)	# random payoffs from uniform distribution
  
  payoffs<-2*payoffs/max(payoffs)
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
    
    learnableTraits<-getLearnableTraits(repertoires, ind, adj_matrix)
    SLpay[t]<-NA
    if (length(learnableTraits)>0){  #only try to learn if there's anything to learn for this agent
      if (r<params$S) {  # social learning
        selectedTrait<-learnSocially(params, repertoires, ind, adj_matrix, learningStrategy, popAge, payoffs)														
        if (length(selectedTrait)==1){
          ######## calculate payoffs of learning
          focalPay<-0
          if (selectedTrait%in%learnableTraits) {
            repertoires[ind,selectedTrait]<-1
            focalPay<-payoffs[selectedTrait]
          }
          SLpay[t]<-focalPay
        }
        
      }
      else {	# individual learning (=innovation)
        selectedTrait<- learnableTraits[1]
        if (length(learnableTraits)>1) selectedTrait<-sample(learnableTraits,1)
        repertoires[ind,selectedTrait]<-1									
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
  ### add summary statistics to the overall master matrix
  ## number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
  ## characterize mean payoffs for a strategy as the last 10% of timesteps in the simulation
  #summThisSimulation<-c(num_nodes, branching_factor, learningStrategy, 
  #	repl, mean(SLpay[round(timesteps*0.9):timesteps], na.rm=TRUE))
  
  summThisSimulation<-c(params$num_nodes, 
                        branching_factor, 
                        params$tree_layers, 
                        params$alpha1,
                        params$beta1,
                        learningStrategy,
                        params$olderPref,
                        repl,
                        mean(SLpay[round(params$timesteps*0.9):params$timesteps], na.rm=TRUE))
  
  
  return(summThisSimulation)
}