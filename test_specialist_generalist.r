## Specialists vs Generalists
## Hannah Armstrong
## Updated: 09-01-24

## Libraries
library(igraph)
# library(colorRamps)
# library(extraDistr)
# library(matrixStats)

## 1. GENERATE TRAIT MODELS

# Number of nodes includes the root node
# Branching factor can be maximum num_nodes-1

# Trait model showing specialist vs generalist
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
  learnableTraits <- c()
  for (trait in unknownTraits){
    parent <- which(adj_matrix[,trait] == 1)
    if (prod(repertoires[ind, parent] == 1)){
      learnableTraits <- c(learnableTraits, trait)
    }
  }
  
  # If there are any negative mutual relationships between traits, remove them from the set of learnable traits
  for (trait in learnableTraits){
    blocker <- which(adj_matrix[,trait] == -1)
    if (sum(repertoires[ind,blocker]) > 0){
      learnableTraits<-learnableTraits[!learnableTraits==trait]
    }
  }
  
  return(learnableTraits)
}

## 3. SOCIAL LEARNING 

learnSocially <- function(repertoires, ind, adj_matrix, learningStrategy, M, popAge){
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
     if (learningStrategy == 2){
      # Check for all agents how similar they are to self
      for (mod in observedModels){
        simToFocal <- 0
        for (k in 1:num_nodes){ # Loop over all traits and sum similarity
          if (repertoires[ind,k] == repertoires[mod,k]){
            simToFocal <- simToFocal + 1
          }
        }
        wList<-c(wList, simToFocal)
      }
    }
    ##################	
    
    ######	STRATEGY 3: Age-Based Social Learning #####				
    if (learningStrategy == 3){
      # Based on age similarity
      # Check for all agents how similar they are to self
      for (mod in observedModels){
        w <- 10^-8
        ageDif <- popAge[mod] - popAge[ind]								
        if (ageDif >= 0){
          w <- 0.5^ageDif
        }
        wList <- c(wList, w)
      }			
    }	
    ################
    
    ######	STRATEGY 4: Conformist Social Learning #####				
    if (learningStrategy == 4){
      # Count the selected behaviours and weigh common ones more
      for (mod in 1:length(observedBehaviours)){
        w <- length(which(observedBehaviours == observedBehaviours[mod]))
        wList <- c(wList, w)
      }			
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
branching_factor = 1 # c(1,2, 4, 8, 16, 32, 64, 128)
SLS = 1 # c(1, 2, 3, 4, 0) (0 = random, 1 = payoff-based, 2 = similarity-based, 3 = age-based, 4 = conformity)
SL_rate = 0.99
reset_rate = 0.01
t_max = 5000
r_max = 1

## 5. SIMULATION

# Functions needed for this simulation:
# 1. generate_specialist_generalist
# 2. initializePopulation
# 3. assignAges
# 4. learnSocially
  
# Summary matrix with success of learning strategies 
# Simulation replicate, number of nodes, branching factor, learning strategy, payoff at the end of the simulation
strategySuccess <- matrix(nrow = 0, ncol = 5)
colnames(strategySuccess) <- c("Simulation", "Nodes", "Branching", "SLS", "Total Payoff")

# Loop over social learning strategies 
for(SLS in 0:4){
  
  # Bookkeeping overall summaries
  summMeanTraitsInSystem <- matrix(nrow = 0, ncol = t_max)
  summSLSPayoff <- matrix(nrow = 0, ncol = t_max)
  
  #Loop over replications
  for(r in 1:r_max){
    
    # Show simulation progress
    flush.console()
    print(paste('Replication =', r, ' Number of nodes =', num_nodes, ' SLS =', SLS, 
                ' Branching factor =', branching_factor))
    
    # Create trait model
    trait_model <- generate_specialist_generalist_tree(num_nodes, branching_factor)
    
    # Adjacency matrix of traits
    adj_matrix <- as_adjacency_matrix(trait_model, sparse = FALSE)
    # Root trait (at position 1,1) is its own parent
    adj_matrix[1,1] <- 1
    
    # Node depths 			
    nodeDepths <- 1 + distances(trait_model, v = 1, to = V(trait_model), mode = "out")
    maxNodeDepth <- max(nodeDepths)
    
    # Bookkeeping individual replications
    SLSPayoff <- rep(NA, t_max) # SLS payoff at each timestep
    meanTraitsInSystem <- rep(NA, t_max) # Number of traits in population at each timestep
    
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
    
    # Create matrix to track number of individuals with each trait
    traitSums <- colSums(popn)
    traitTracking <- matrix(traitSums[-1], nrow = branching_factor, ncol = (num_nodes - 1)/branching_factor)
    traitDiagram <- matrix(2:num_nodes, nrow = branching_factor, ncol = (num_nodes - 1)/branching_factor)
    
    # Loop over timesteps 
    for(t in 1:t_max){
      
      # Sample an individual
      ind <- sample(1:N, 1)
      
      SLSPayoff[t] <- NA
      
      # What are the learnable traits?
      learnableTraits <- getLearnableTraits(popn, ind, adj_matrix)
      if(length(learnableTraits > 0)){ # If there are traits to learn
        
        # Do they learn socially or individually?
        if(runif(1) < SL_rate){ # Social learning
          selectedTrait <- learnSocially(popn, ind, adj_matrix, SLS, M, popAge)
          
          if(length(selectedTrait == 1)){ # If there is a potential trait 
            if(selectedTrait %in% learnableTraits){ # If the trait is learnable
              popn[ind, selectedTrait] <- 1
              traitPayoff <- payoffs[selectedTrait]
              SLSPayoff[t] <- traitPayoff
            }
            else{
              SLSPayoff[t] <- 0
            }
          }
          else{
            SLSPayoff[t] <- 0
          }
        }
        else{ # Individual learning
          selectedTrait <- sample(learnableTraits, 1)
          popn[ind, selectedTrait] <- 1
          SLSPayoff[t] <- 0
        }
      }
      else{
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
    # Bookkeeping each replication
    summSLSPayoff <- rbind(summSLSPayoff, SLSPayoff)
    summMeanTraitsInSystem <- rbind(summMeanTraitsInSystem, meanTraitsInSystem)
    
    # Overall summaries
    summThisSimulation <- c(r, num_nodes, branching_factor, SLS, sum(summSLSPayoff[r,]))
    strategySuccess <- rbind(strategySuccess, summThisSimulation)
  }
}

# Export summary statistics
write.csv(strategySuccess, file = "SummaryStats_bf64")

# Plotting
boxplot(Payoff ~ SLS, data = strategySuccess, main = "Mean Payoff for each SLS (bf = 64)")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

## 6. PRELIMINARY RESULTS

bf1 <- read.csv("SummaryStats_bf1")
bf2 <- read.csv("SummaryStats_bf2")
bf4 <- read.csv("SummaryStats_bf4")
bf8 <- read.csv("SummaryStats_bf8")
bf16 <- read.csv("SummaryStats_bf16")
bf32 <- read.csv("SummaryStats_bf32")
bf64 <- read.csv("SummaryStats_bf64")
bf128 <- read.csv("SummaryStats_bf128")

dev.new()
par(mfrow=c(2,4))
boxplot(Payoff ~ SLS, data = bf1, main = "bf = 1", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf2, main = "bf = 2", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf4, main = "bf = 4", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf8, main = "bf = 8", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf16, main = "bf = 16", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf32, main = "bf = 32", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf64, main = "bf = 64", ylab = "Total Payoff")
boxplot(Payoff ~ SLS, data = bf128, main = "bf = 128", ylab = "Total Payoff")
