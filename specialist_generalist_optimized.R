## Specialists vs Generalists Final Simulation
## Hannah Armstrong

## LIBRARIES

library(igraph)
library(ggplot2)
library(reshape2)
library(purrr)
library(furrr)
library(future)
library(parallelly)

## FUNCTIONS

# Generating the tree (specialist vs generalist)

# Number of nodes includes the root node
# Branching factor can be maximum num_nodes-1

generate_specialist_generalist_tree <- function(num_nodes, branching_factor) {
  g <- graph.empty(n = num_nodes, directed = TRUE)
  edgeList <- c()
  
  # Connect node 1 to number of nodes specified by the branching factor 
  for (i in 2:(branching_factor + 1)) {
    edgeList <- c(edgeList, 1, i)
  }
  
  # Connect each subsequent node to only one other node
  if (branching_factor < num_nodes-1){
    for (i in 2:(num_nodes-branching_factor)) {
      edgeList <- c(edgeList, i, i + branching_factor)
    }
  }  
  g <- add_edges(g, c(edgeList))
  return(g)
}

## 2. GENERATE POPULATION

initializePopulation <- function(N, num_nodes, adj_matrix){
  # Start with empty repertoires (but fill them up in the next step)
  repertoires <- matrix(0, nrow = N, ncol = num_nodes)
  # Everyone has the root trait of the cultural system
  repertoires[,1] <- 1
  
  # Initialize the population with repertoires (all connected to the tree root) 
  # With sizes drawn from a uniform distribution between 1:num_nodes
  for (ind in 1:N){
    numTraits <- sample(1:num_nodes, 1)	# Number of traits of this agent
    for (tr in 2:numTraits){			# Add (randomly chosen) learnable traits
      unknownTraits <- which(repertoires[ind,] == 0)
      # For which of these traits is the parent trait in the repertoire?
      # These are the traits currently 'learnable' to the individual
      learnableTraits <- c()
      for (trait in unknownTraits){
        parent <- which(adj_matrix[,trait] == 1)
        if (prod(repertoires[ind, parent] == 1)){
          learnableTraits <- c(learnableTraits, trait)
        }
      }
      
      if (length(learnableTraits) == 1){
        repertoires[ind, learnableTraits] <- 1
      }
      if (length(learnableTraits) > 1){
        addTrait <- sample(learnableTraits, 1)
        repertoires[ind, addTrait] <- 1
      }
    }
  }
  return(repertoires)
}

assignAges <- function(repertoires){
  # For age-based social learning, we need to assume initial ages. 
  # Assume the age is proportional to the repertoire size
  N <- nrow(repertoires)	
  popAge <- rep(0, N)
  for (ind in 1:N) {
    popAge[ind] <- sum(repertoires[ind,])
  }
  return (popAge)
}

getLearnableTraits <- function(repertoires, ind, adj_matrix){
  # Which traits are currently not in the individual's repertoire?
  unknownTraits <- which(repertoires[ind,] == 0)
  
  # For which of these traits is the parent trait in the repertoire?
  # These are the traits currently 'learnable' to the individual
  
  
  learnableTraits <- vapply(unknownTraits, function(trait) {
    parent <- which(adj_matrix[,trait] == 1)
    all(repertoires[ind, parent] == 1)
  }, logical(1))
  
  return(unknownTraits[learnableTraits])
}



getEnvironmentalLearnability <- function(repertoires, adj_matrix){
  p <- c()
  
  for(ind in 1:nrow(repertoires)){
    
    knownTraits <- which(repertoires[ind,] == 1)
    
    unknownTraits <- which(repertoires[ind,] == 0)
    
    if(length(unknownTraits) == 0){
      p[ind] <- NA 
      next
    }
    
    learnableTraits <- getLearnableTraits(repertoires, ind, adj_matrix)
    
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

# Environmental learnability function that's faster than the one above
getEnvironmentalLearnability2 <- function(repertoires, adj_matrix) {
  # Calculate known and unknown traits for all individuals
  knownTraits <- apply(repertoires, 1, function(ind) which(ind == 1))
  unknownTraits <- apply(repertoires, 1, function(ind) which(ind == 0))
  
  # Initialize a vector to store the probabilities
  p <- numeric(nrow(repertoires))
  
  # Function to calculate learnable traits for an individual
  getLearnable <- function(ind) {
    getLearnableTraits(repertoires, ind, adj_matrix)
  }
  
  # Calculate learnable traits for all individuals
  learnableTraits <- lapply(1:nrow(repertoires), getLearnable)
  
  # Function to calculate the frequency of traits in a subset of the population
  traitFrequency <- function(traits, pop) {
    colSums(pop[, traits, drop = FALSE])
  }
  
  # Calculate probabilities for each individual
  for (ind in 1:nrow(repertoires)) {
    if (length(unknownTraits[[ind]]) == 0) {
      p[ind] <- NA
      next
    }
    
    popOthers <- repertoires[-ind, , drop = FALSE]
    
    freqLearnable <- sum(traitFrequency(learnableTraits[[ind]], popOthers))
    freqUnknown <- sum(traitFrequency(unknownTraits[[ind]], popOthers))
    
    p[ind] <- freqLearnable / freqUnknown
  }
  
  # Return the mean probability, excluding NA values
  return(mean(p, na.rm = TRUE))
}

## 3. SOCIAL LEARNING 

learnSocially <- function(repertoires, ind, adj_matrix, learningStrategy, M, popAge, N, num_nodes, payoffs){
  
  # Sample M random other individuals
  pool <- 1:N
  poolOthers <- pool[-ind] # Agents do not sample themselves
  models <- sample(poolOthers, M, replace = FALSE)
  
  # Randomly pick 1 trait from each model
  # Only consider traits the learning agent do not know yet
  observedBehaviours <- c()
  observedModels <- c()
  for (model in models){
    newTraits <- c()
    for (k in 1:num_nodes){
      if (repertoires[model,k] == 1 && repertoires[ind,k] == 0){
        newTraits <- c(newTraits, k)
      }
    }
    if (length(newTraits) > 1) {
      tr <- sample(newTraits, 1)
      observedBehaviours <- c(observedBehaviours, tr)
      observedModels <- c(observedModels, model)
    }
    if (length(newTraits) == 1) {
      observedBehaviours <- c(observedBehaviours, newTraits)
      observedModels <- c(observedModels, model)
    }
  }
  
  # Check if the vectors are filled as expected
  observedBehaviours
  observedModels
  
  # If there's no trait to learn among the observed ones, skip
  # SLpay was initialized at NA so nothing happens in that case
  # If there IS something to learn:
  if (length(observedBehaviours) > 0){
    # List of weights of each observed behaviour
    # Weights depend on learning strategy
    # Normalized weights are used for choice
    wList <- c()
    selectedTrait <- c()	
    
    ##### STRATEGY 1: Payoff-Based Social Learning #####
    if (learningStrategy == 1){
      wList <- payoffs[observedBehaviours] / sum(payoffs[observedBehaviours])
    }
    ##################
    
    ###### STRATEGY 2: Similarity-Based Social Learning ######
    if (learningStrategy == 2) {
      for (model in observedModels) {
        modelIndex <- which(observedModels == model)
        wList[modelIndex] <- sum(repertoires[ind, ] == repertoires[model, ]) / ncol(repertoires)
      }
    }
    
    ##################	
    
    ######	STRATEGY 3: Age-Based Social Learning #####				
    if (learningStrategy == 3){
      # Based on age similarity
      # Check for all agents how similar they are to self
      for (mod in observedModels){
        ageDif <- popAge[mod] - popAge[ind]								
        w <- 0.5^ageDif
        wList <- c(wList, w)
      }
      wList <- wList / sum(wList)
    }	
    ################
    
    ######	STRATEGY 4: Conformist Social Learning #####				
    if (learningStrategy == 4){
      # Count the selected behaviours and weigh common ones more
      for (mod in 1:length(observedBehaviours)){
        w <- length(which(observedBehaviours == observedBehaviours[mod]))
        wList <- c(wList, w)
      }	
      wList <- wList / sum(wList)
    }
    ################
    
    ###### STRATEGY 0: Random Learning
    if (learningStrategy == 0){
      # Select a trait you don't have at random
      wList <- rep(1, length(observedBehaviours))							
    }
    ############
    
    ### MAKE CHOICE ###
    selectedTrait <- ifelse(length(observedBehaviours) == 1, observedBehaviours[1], sample(observedBehaviours, 1, prob = wList))
    
    return(selectedTrait)	
  }						
}

## 4. PARAMETERS

N = 100
M = 10
num_nodes = 129 # (including root node)
branching_factor = 128 # c(1,2, 4, 8, 16, 32, 64, 128)
SLS = 1 # c(1, 2, 3, 4, 0) (0 = random, 1 = payoff-based, 2 = similarity-based, 3 = age-based, 4 = conformity)
SL_rate = 0.99
reset_rate = 0.01
t_max = 20000
r_max = 1000

## 5. SIMULATION

# Functions needed for this simulation:
# 1. generate_specialist_generalist
# 2. verticesInBranches
# 3. initializePopulation
# 4. assignAges
# 5. getLearnableTraits
# 6. getEnvironmentalLearnability
# 7. learnSocially

# Summary matrix with success of learning strategies 
# Simulation replicate, number of nodes, branching factor, learning strategy, payoff at the end of the simulation, successful trials


# Bookkeeping overall summaries
## summMeanTraitsInBranch <- list()
## summVarAcrossBranch <- matrix(nrow = 0, ncol = t_max)
## summProbabilities <- matrix(nrow = 0, ncol = t_max)


# Loop over social learning strategies 

run_simulation <- function(N, M, num_nodes, branching_factor, SLS, SL_rate, reset_rate, t_max, r){

  # Bookkeeping individual replications
  SLSPayoff <- rep(NA, t_max) # SLS payoff at each timestep
  meanTraitsInSystem <- rep(NA, t_max) # Number of traits in population at each timestep
  ## probabilities <- rep(NA, t_max) # Environmental learnability
  ## meanTraitsInBranch <- matrix(nrow = 0, ncol = branching_factor) # Mean traits in each branch
  ## varTraitsAcrossBranch <- c() # Variance in mean traits
  
  # Create trait model
  trait_model <- generate_specialist_generalist_tree(num_nodes, branching_factor)
  
  # Adjacency matrix of traits
  adj_matrix <- as_adjacency_matrix(trait_model, sparse = FALSE)
  # Root trait (at position 1,1) is its own parent
  adj_matrix[1,1] <- 1
  
  # Vertices in each branch
  ## branches <- verticesInBranches(trait_model)
  ## vertBranches <- do.call(cbind, branches)
  ## vertBranches <- vertBranches[-1,]
  
  # Node depths 			
  nodeDepths <- 1 + distances(trait_model, v = 1, to = V(trait_model), mode = "out")
  maxNodeDepth <- max(nodeDepths)
  
  # Set payoffs
  #	payoffs <- rep(1, num_nodes) # Equal uniform 
  # payoffs <- runif(num_nodes)	# Random payoffs from uniform distribution
  payoffs <- runif(num_nodes) * nodeDepths # Payoffs increase with depth
  #	payoffs <- runif(num_nodes) * (max(nodeDepths) - nodeDepths + 1) # Payoffs decrease with depth
  payoffs <- 2 * payoffs / max(payoffs) # I believe this is used to make the payoffs all between 0-2
  
  # Initialize the population
  popn <- initializePopulation(N, num_nodes, adj_matrix)
  
  # Assign ages
  popAge <- assignAges(popn)
  
  # Loop over timesteps 
  for(t in 1:t_max){
    
    ##probabilities[t] <- getEnvironmentalLearnability(popn, adj_matrix)
    
    # Mean number of traits per branch (over all individuals, not per individual)
    ## meanKnownTraitsBranch <- c()
    
    ## if(branching_factor == 1){ # Completely constrained (one branch)
    ##  meanKnownTraitsBranch <- sum(popn)/(num_nodes-1)
    ##  varTraitsBranch <- NA
    ##} else if (branching_factor == (num_nodes-1)){ # Completely unconstrained (independent traits)
    ## 
    ##   for(trait in vertBranches){
    ##    meanKnownTraitsBranch[trait] <- colSums(popn)[trait]
    ##   }
    ##  meanKnownTraitsBranch <- na.omit(meanKnownTraitsBranch)
    ##  varTraitsBranch <- var(meanKnownTraitsBranch) # Variance between the branches
    ##} else { # All other branching factors other than 1 or 128
    ##  for(col in 1:branching_factor){
    ##    subset <- vertBranches[,col]
    ##    
    ##    traitsInBranch <- 0
    ##    
    ##    for(trait in subset){
    ##      traitsInBranch <- traitsInBranch + colSums(popn)[trait]
    ##    }
    ##    
    ##    meanKnownTraitsBranch[col] <- traitsInBranch / ((num_nodes-1)/branching_factor)
    ##  }
    ##  
    # Variance between the branches
    ##  varTraitsBranch <- var(meanKnownTraitsBranch)
    ##}
    
    ##meanTraitsInBranch <- rbind(meanTraitsInBranch, meanKnownTraitsBranch)
    ##varTraitsAcrossBranch <- c(varTraitsAcrossBranch, varTraitsBranch)
    
    # Sample an individual
    ind <- sample(1:N, 1)
    
    SLSPayoff[t] <- NA
    
    # What are the learnable traits?
    learnableTraits <- getLearnableTraits(popn, ind, adj_matrix)
    if(length(learnableTraits > 0)){ # If there are traits to learn
      
      # Do they learn socially or individually?
      if(runif(1) < SL_rate){ # Social learning
        selectedTrait <- learnSocially(popn, ind, adj_matrix, SLS, M, popAge, N, num_nodes, payoffs)
        
        if(length(selectedTrait == 1)){ # If there is a potential trait 
          if(selectedTrait %in% learnableTraits){ # If the trait is learnable
            popn[ind, selectedTrait] <- 1
            traitPayoff <- payoffs[selectedTrait]
            SLSPayoff[t] <- traitPayoff
          } 
          else {
            SLSPayoff[t] <- 0
          }
        } 
        else {
          SLSPayoff[t] <- 0
        }
      } 
      else { # Individual learning
        selectedTrait <- sample(learnableTraits, 1)
        popn[ind, selectedTrait] <- 1
        SLSPayoff[t] <- 0
      }
    } 
    else {
      SLSPayoff[t] <- 0
    }
    # Increase agent age
    popAge[ind] <- popAge[ind] + 1
    
    # Replace with a naive individual
    if(runif(1) < reset_rate){
      popn[ind,] <- c(1, rep(0, num_nodes - 1)) # Reset repertoire
      popAge[ind] <- 0 # Reset age
    }
    # Bookkeeping each timestep
    meanTraitsInSystem[t] <- sum(popn) / (num_nodes * N)
  }
  

  ##summMeanTraitsInBranch <- c(summMeanTraitsInBranch, list(meanTraitsInBranch))
  ##summVarAcrossBranch <- rbind(summVarAcrossBranch, varTraitsAcrossBranch)
  ##summProbabilities <- rbind(summProbabilities, probabilities)
  
  # Overall summaries
  summThisSimulation <- c(r, num_nodes, branching_factor, SLS, sum(SLSPayoff), sum(SLSPayoff>0)/t_max)
  return(list(summThisSimulation, SLSPayoff, meanTraitsInSystem))
}

run_all_simulations <- function(parameters) {
  print(paste("starting", nrow(parameters), "simulations..."))
  
  results <- purrr::pmap(parameters, function(N, M, num_nodes, branching_factor, SLS, SL_rate, reset_rate, t_max, r) {
    print(paste('Replication =', r, ' Number of nodes =', num_nodes, ' SLS =', SLS, 
                ' Branching factor =', branching_factor))
    
    summThisSimulation <- run_simulation(N, M, num_nodes, branching_factor, SLS, SL_rate, reset_rate, t_max, r)
  })

  strategySuccess <- do.call(rbind, lapply(results, `[[`, 1))
  colnames(strategySuccess) <- c("Simulation", "Nodes", "Branching", "SLS", "TotalPayoff", "ProportionSuccessfulTrials")
  
  summSLSPayoff <- do.call(rbind, lapply(results, `[[`, 2))
  summMeanTraitsInSystem <- do.call(rbind, lapply(results, `[[`, 3))
  return(list(strategySuccess, summSLSPayoff, summMeanTraitsInSystem))
}


run_all_simulations_parallel <- function(parameters) {
  print(paste("starting", nrow(parameters), "simulations in parallel..."))
  
  results <- furrr::future_pmap(parameters, function(N, M, num_nodes, branching_factor, SLS, SL_rate, reset_rate, t_max, r) {

    summThisSimulation <- run_simulation(N, M, num_nodes, branching_factor, SLS, SL_rate, reset_rate, t_max, r)
  },
  .options = furrr::furrr_options(seed = TRUE)   
  )
  strategySuccess <- do.call(rbind, lapply(results, `[[`, 1))
  colnames(strategySuccess) <- c("Simulation", "Nodes", "Branching", "SLS", "TotalPayoff", "ProportionSuccessfulTrials")
  
  summSLSPayoff <- do.call(rbind, lapply(results, `[[`, 2))
  summMeanTraitsInSystem <- do.call(rbind, lapply(results, `[[`, 3))
  
  return(list(strategySuccess, summSLSPayoff, summMeanTraitsInSystem))
}





## You can add both ranges of parameters and individual values, and the
## simulation will run for all combinations
parameters <- expand.grid(
  N = 100,
  M = 10,
  num_nodes = 129,
  branching_factor = 128,
  SLS = 0:4,
  SL_rate = 0.99,
  reset_rate = 0.01,
  t_max = 20000,
  r = 1
)
### Run sequentially 
results <- run_all_simulations(parameters)

### Run in parallel 
plan(multisession, workers = parallelly::availableCores(omit = 1)) # omit 1 if you want to keep using your computer during the simulation
system.time(results <- run_all_simulations_parallel(parameters))

### Unpack results
strategySuccess <- results[[1]]
summSLSPayoff <- results[[2]]
summMeanTraitsInSystem <- results[[3]]


# Export summary statistics
write.csv(strategySuccess, file = "StrategySuccess_Payoff")
# write.csv(summProbabilities, file = "EnvironmentalLearnability")
write.csv(summSLSPayoff, file = "SLSPayoff_Payoff")
write.csv(summMeanTraitsInSystem, file = "MeanTraitsInSystem_Payoff")
# write.csv(summVarAcrossBranch, file = "VarianceAcrossBranch")
# saveRDS(summMeanTraitsInBranch, file = "MeanTraitsInBranch.RData")
