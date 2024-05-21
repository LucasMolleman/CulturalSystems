## Specialists vs Generalists
## Hannah Armstrong

## Libraries
library(igraph)
# library(colorRamps)
# library(extraDistr)
# library(matrixStats)
library(ggplot2)
library(reshape2)

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

# Determine the nodes in each branch
verticesInBranches <- function(graph) {
  branches <- list()
  visited <- logical(vcount(graph))
  
  dfs <- function(vertex, branch) {
    visited[vertex] <<- TRUE
    branch <- c(branch, vertex)
    neighbors <- neighbors(graph, vertex)
    unvisited_neighbors <- neighbors[!visited[neighbors]]
    if (length(unvisited_neighbors) > 0) {
      for (neighbor in unvisited_neighbors) {
        dfs(neighbor, branch)
      }
    } else {
      branches <<- c(branches, list(branch))
    }
  }
  
  for (vertex in 1:vcount(graph)) {
    if (!visited[vertex]) {
      dfs(vertex, numeric(0))
    }
  }
  
  return(branches)
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
      wList <- wList / sum(wList)
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
branching_factor = 4 # c(1,2, 4, 8, 16, 32, 64, 128)
SLS = 0 # c(1, 2, 3, 4, 0) (0 = random, 1 = payoff-based, 2 = similarity-based, 3 = age-based, 4 = conformity)
SL_rate = 0.99
reset_rate = 0.01
t_max = 20000
r_max = 1

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
# Simulation replicate, number of nodes, branching factor, learning strategy, payoff at the end of the simulation
strategySuccess <- matrix(nrow = 0, ncol = 6)
colnames(strategySuccess) <- c("Simulation", "Nodes", "Branching", "SLS", "TotalPayoff", "ProportionSuccessfulTrials")

# Bookkeeping overall summaries
summMeanTraitsInSystem <- matrix(nrow = 0, ncol = t_max)
summSLSPayoff <- matrix(nrow = 0, ncol = t_max)
summMeanTraitsInBranch <- list()
summVarAcrossBranch <- matrix(nrow = 0, ncol = t_max)
summProbabilities <- matrix(nrow = 0, ncol = t_max)

Sys.time()

# Loop over social learning strategies 
for(SLS in 0:4){
  
  #Loop over replications
  for(r in 1:r_max){
    
    # Show simulation progress
    flush.console()
    print(paste('Replication =', r, ' Number of nodes =', num_nodes, ' SLS =', SLS, 
                ' Branching factor =', branching_factor))
    
    # Bookkeeping individual replications
    SLSPayoff <- rep(NA, t_max) # SLS payoff at each timestep
    meanTraitsInSystem <- rep(NA, t_max) # Number of traits in population at each timestep
    probabilities <- rep(NA, t_max) # Environmental learnability
    meanTraitsInBranch <- matrix(nrow = 0, ncol = branching_factor) # Mean traits in each branch
    varTraitsAcrossBranch <- c() # Variance in mean traits
    
    # Create trait model
    trait_model <- generate_specialist_generalist_tree(num_nodes, branching_factor)
    
    # Adjacency matrix of traits
    adj_matrix <- as_adjacency_matrix(trait_model, sparse = FALSE)
    # Root trait (at position 1,1) is its own parent
    adj_matrix[1,1] <- 1
    
    # Vertices in each branch
    branches <- verticesInBranches(trait_model)
    vertBranches <- do.call(cbind, branches)
    vertBranches <- vertBranches[-1,]
    
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
      
      if(t %% 50 == 0){
        print(paste('Time = ', t))
      }
      
      probabilities[t] <- getEnvironmentalLearnability(popn, adj_matrix)
      
      # Mean number of traits per branch (over all individuals, not per individual)
      meanKnownTraitsBranch <- c()
      
      if(branching_factor == 1){ # Completely constrained (one branch)
        meanKnownTraitsBranch <- sum(popn)/(num_nodes-1)
        varTraitsBranch <- NA
      } else if (branching_factor == (num_nodes-1)){ # Completely unconstrained (independent traits)
       
         for(trait in vertBranches){
          meanKnownTraitsBranch[trait] <- colSums(popn)[trait]
         }
        meanKnownTraitsBranch <- na.omit(meanKnownTraitsBranch)
        varTraitsBranch <- var(meanKnownTraitsBranch) # Variance between the branches
      } else { # All other branching factors other than 1 or 128
        for(col in 1:branching_factor){
          subset <- vertBranches[,col]
          
          traitsInBranch <- 0
          
          for(trait in subset){
            traitsInBranch <- traitsInBranch + colSums(popn)[trait]
          }
          
          meanKnownTraitsBranch[col] <- traitsInBranch / ((num_nodes-1)/branching_factor)
        }
        
        # Variance between the branches
        varTraitsBranch <- var(meanKnownTraitsBranch)
      }
      
      meanTraitsInBranch <- rbind(meanTraitsInBranch, meanKnownTraitsBranch)
      varTraitsAcrossBranch <- c(varTraitsAcrossBranch, varTraitsBranch)
      
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
    
    # Bookkeeping each replication
    summSLSPayoff <- rbind(summSLSPayoff, SLSPayoff)
    summMeanTraitsInSystem <- rbind(summMeanTraitsInSystem, meanTraitsInSystem)
    summMeanTraitsInBranch <- c(summMeanTraitsInBranch, list(meanTraitsInBranch))
    summVarAcrossBranch <- rbind(summVarAcrossBranch, varTraitsAcrossBranch)
    summProbabilities <- rbind(summProbabilities, probabilities)
    
    # Overall summaries
    summThisSimulation <- c(r, num_nodes, branching_factor, SLS, sum(SLSPayoff), sum(SLSPayoff>0)/t_max)
    strategySuccess <- rbind(strategySuccess, summThisSimulation)
  }
  print(Sys.time())
}

# Export summary statistics
write.csv(strategySuccess, file = "StrategySuccess")
write.csv(summProbabilities, file = "EnvironmentalLearnability")
write.csv(summSLSPayoff, file = "SLSPayoff")
write.csv(summMeanTraitsInSystem, file = "MeanTraitsInSystem")
write.csv(summVarAcrossBranch, file = "VarianceAcrossBranch")
saveRDS(summMeanTraitsInBranch, file = "MeanTraitsInBranch.RData")

# 6. PLOTTING

## 30-03-24 Simulation Results ##

bf1 <- read.csv("SummaryStats_bf1")
bf2 <- read.csv("SummaryStats_bf2")
bf4 <- read.csv("SummaryStats_bf4")
bf8 <- read.csv("SummaryStats_bf8")
bf16 <- read.csv("SummaryStats_bf16")
bf32 <- read.csv("SummaryStats_bf32")
bf64 <- read.csv("SummaryStats_bf64")
bf128 <- read.csv("SummaryStats_bf128")

random <- rbind(bf1[bf1$SLS == 0,], bf2[bf2$SLS == 0,], bf4[bf4$SLS == 0,], bf8[bf8$SLS == 0,],
                bf16[bf16$SLS == 0,], bf32[bf32$SLS == 0,], bf64[bf64$SLS == 0,], bf128[bf128$SLS == 0,])
randomtotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(random$Payoff[random$Branching == 1]), mean(random$Payoff[random$Branching == 2]),
                                                    mean(random$Payoff[random$Branching == 4]), mean(random$Payoff[random$Branching == 8]),
                                                    mean(random$Payoff[random$Branching == 16]), mean(random$Payoff[random$Branching == 32]),
                                                    mean(random$Payoff[random$Branching == 64]), mean(random$Payoff[random$Branching == 128])))
randomtotal <- as.data.frame(randomtotal)
colnames(randomtotal) <- c("BranchingFactor", "MeanPayoff")
randomtotal$BranchingFactor <- factor(randomtotal$BranchingFactor, levels = rev(unique(randomtotal$BranchingFactor)))

payoff <- rbind(bf1[bf1$SLS == 1,], bf2[bf2$SLS == 1,], bf4[bf4$SLS == 1,], bf8[bf8$SLS == 1,],
                bf16[bf16$SLS == 1,], bf32[bf32$SLS == 1,], bf64[bf64$SLS == 1,], bf128[bf128$SLS == 1,])
payofftotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(payoff$Payoff[payoff$Branching == 1]), mean(payoff$Payoff[payoff$Branching == 2]),
                                                    mean(payoff$Payoff[payoff$Branching == 4]), mean(payoff$Payoff[payoff$Branching == 8]),
                                                    mean(payoff$Payoff[payoff$Branching == 16]), mean(payoff$Payoff[payoff$Branching == 32]),
                                                    mean(payoff$Payoff[payoff$Branching == 64]), mean(payoff$Payoff[payoff$Branching == 128])))
payofftotal <- as.data.frame(payofftotal)
payofftotal[,2] <- payofftotal[,2]/randomtotal[,2]
colnames(payofftotal) <- c("BranchingFactor", "MeanPayoff (divided by Random)")
payofftotal$BranchingFactor <- factor(payofftotal$BranchingFactor, levels = rev(unique(payofftotal$BranchingFactor)))


similarity <- rbind(bf1[bf1$SLS == 2,], bf2[bf2$SLS == 2,], bf4[bf4$SLS == 2,], bf8[bf8$SLS == 2,],
                    bf16[bf16$SLS == 2,], bf32[bf32$SLS == 2,], bf64[bf64$SLS == 2,], bf128[bf128$SLS == 2,])
similaritytotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(similarity$Payoff[similarity$Branching == 1]), mean(similarity$Payoff[similarity$Branching == 2]),
                                                        mean(similarity$Payoff[similarity$Branching == 4]), mean(similarity$Payoff[similarity$Branching == 8]),
                                                        mean(similarity$Payoff[similarity$Branching == 16]), mean(similarity$Payoff[similarity$Branching == 32]),
                                                        mean(similarity$Payoff[similarity$Branching == 64]), mean(similarity$Payoff[similarity$Branching == 128])))
similaritytotal <- as.data.frame(similaritytotal)
similaritytotal[,2] <- similaritytotal[,2]/randomtotal[,2]
colnames(similaritytotal) <- c("BranchingFactor", "MeanPayoff (divided by Random)")
similaritytotal$BranchingFactor <- factor(similaritytotal$BranchingFactor, levels = rev(unique(similaritytotal$BranchingFactor)))

age <- rbind(bf1[bf1$SLS == 3,], bf2[bf2$SLS == 3,], bf4[bf4$SLS == 3,], bf8[bf8$SLS == 3,],
             bf16[bf16$SLS == 3,], bf32[bf32$SLS == 3,], bf64[bf64$SLS == 3,], bf128[bf128$SLS == 3,])
agetotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(age$Payoff[age$Branching == 1]), mean(age$Payoff[age$Branching == 2]),
                                                 mean(age$Payoff[age$Branching == 4]), mean(age$Payoff[age$Branching == 8]),
                                                 mean(age$Payoff[age$Branching == 16]), mean(age$Payoff[age$Branching == 32]),
                                                 mean(age$Payoff[age$Branching == 64]), mean(age$Payoff[age$Branching == 128])))
agetotal <- as.data.frame(agetotal)
agetotal[,2] <- agetotal[,2]/randomtotal[,2]
colnames(agetotal) <- c("BranchingFactor", "MeanPayoff (divided by Random")
agetotal$BranchingFactor <- factor(agetotal$BranchingFactor, levels = rev(unique(agetotal$BranchingFactor)))

conformity <- rbind(bf1[bf1$SLS == 4,], bf2[bf2$SLS == 4,], bf4[bf4$SLS == 4,], bf8[bf8$SLS == 4,],
                    bf16[bf16$SLS == 4,], bf32[bf32$SLS == 4,], bf64[bf64$SLS == 4,], bf128[bf128$SLS == 4,])
conformitytotal <- cbind(c(1,2,4,8,16,32,64,128), rbind(mean(conformity$Payoff[conformity$Branching == 1]), mean(conformity$Payoff[conformity$Branching == 2]),
                                                        mean(conformity$Payoff[conformity$Branching == 4]), mean(conformity$Payoff[conformity$Branching == 8]),
                                                        mean(conformity$Payoff[conformity$Branching == 16]), mean(conformity$Payoff[conformity$Branching == 32]),
                                                        mean(conformity$Payoff[conformity$Branching == 64]), mean(conformity$Payoff[conformity$Branching == 128])))
conformitytotal <- as.data.frame(conformitytotal)
conformitytotal[,2] <- conformitytotal[,2]/randomtotal[,2]
colnames(conformitytotal) <- c("BranchingFactor", "MeanPayoff (divided by Random")
conformitytotal$BranchingFactor <- factor(conformitytotal$BranchingFactor, levels = rev(unique(conformitytotal$BranchingFactor)))

plot(x = as.numeric(payofftotal[,1]), y = payofftotal[,2], type = "l", col = "red", 
     xlab = "Branching Factor", ylab = "Total Payoff Divided by Random Learning",
     main = "Mean payoff for each SLS and branching factor, averaged over 100 Simulations", 
     ylim = c(0.8, 1.6), xaxt = "n")
axis(1, at = 1:length(levels(randomtotal$BranchingFactor)), labels = levels(randomtotal$BranchingFactor))
lines(x = as.numeric(similaritytotal[,1]), y = similaritytotal[,2], type = "l", col = "blue")
lines(x = as.numeric(agetotal[,1]), y = agetotal[,2], type = "l", col = "green")
lines(x = as.numeric(conformitytotal[,1]), y = conformitytotal[,2], type = "l", col = "orange")
legend("topleft", legend = c("Payoff", "Similarity", "Age", "Conformity"),
       col = c("red", "blue", "green", "orange"), lwd = 2, cex = 0.8)


## 13-05-24 Simulation Results ##

# Plotting Mean Traits in the Branches (bf = 4)
data <- readRDS("MeanTraitsInBranch.RData")
random <- data[1:20]
payoff <- data[21:40]
similarity <- data[41:60]
age <- data[61:80]
conformity <- data[81:100]

plot(age[[2]][,1], type = "l", col = "red")
lines(age[[2]][,2], type = "l", col = "blue")
lines(age[[2]][,3], type = "l", col = "green")
lines(age[[2]][,4], type = "l", col = "magenta")

# Variance
variance <- read.csv("VarianceAcrossBranch")
random <- variance[1:20,]
random$X <- factor(1:20)
colnames(random) <- c("Simulation", paste("T", 1:5000, sep = ""))
ranLong <- melt(random, id.vars = "Simulation", variable.name = "Variable", value.name = "Value")

ggplot(ranLong, aes(x = Variable, y = Value, group = Simulation, color = Simulation)) +
  geom_line() +
  theme_minimal() + 
  labs(title = "Variance in Mean Traits Across the Branches",
       x = "Timesteps",
       y = "Variance",
       color = "Simulation")

## 14-05-24 Simulation Results ##

write.csv(summSLSPayoff, file = "SLSPayoff (No Labels)")
sls_payoff <- cbind(labels, summSLSPayoff)
colnames(sls_payoff) <- c("Simulation", "SLS", 1:5000)
rownames(sls_payoff) <- 1:100
write.csv(sls_payoff, file = "SLSPayoff")

write.csv(summMeanTraitsInSystem, file = "MeanTraitsInSystem (No Labels)")
traitsinenv <- cbind(labels, summMeanTraitsInSystem)
colnames(traitsinenv) <- c("Simulation", "SLS", 1:5000)
rownames(traitsinenv) <- 1:100
write.csv(traitsinenv, file = "MeanTraitsInSystem")

saveRDS(summMeanTraitsInBranch, file="MeanTraitsInBranch.RData")
write.csv(summVarAcrossBranch, file = "VarianceAcrossBranch")

write.csv(summProbabilities, file = "EnvironmentalLearnability (No Labels)")
EnvironmentalLearnability <- cbind(labels, summProbabilities)
colnames(EnvironmentalLearnability) <- c("Simulation", "SLS", 1:5000)
rownames(EnvironmentalLearnability) <- 1:100
write.csv(EnvironmentalLearnability, file = "EnvironmentalLearnability")

# Plotting success of SLSs
boxplot(TotalPayoff ~ SLS, data = strategySuccess, main = "Total Payoff")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

# Plotting proportion of successful trials
boxplot(ProportionSuccessfulTrials ~ SLS, data = strategySuccess, main = "Proportion of Successful Traits")
legend('topright', c('0 = Random', '1 = Payoff', '2 = Similarity', '3 = Age', '4 = Conformity'))

# Plotting environmental learnability

# Random learning
matplot(t(summProbabilities), type = "l", xlab = "Timesteps", ylab = "Environmental Learnability", 
        main = "Environmental Learnability")
lines(plot, lwd = 3)

plot1 <- summProbabilities[1:20,]
avy1 <- colMeans(plot1)

matplot(t(plot1), type = "l", xlab = "Timesteps", ylab = "Environmental Learnability", 
        main = "Random Learning", ylim = c(0.09,0.19))
lines(avy1, lwd = 3)

# Payoff based social learning 
plot2 <- summProbabilities[21:40,]
avy2 <- colMeans(plot2)

matplot(t(plot2), type = "l", xlab = "Timesteps", ylab = "Environmental Learnability", 
        main = "Payoff-Based Social Learning", ylim = c(0.09,0.19))
lines(avy2, lwd = 3)

# Similarity based social learning
plot3 <- summProbabilities[41:60,]
avy3 <- colMeans(plot3)

matplot(t(plot3), type = "l", xlab = "Timesteps", ylab = "Environmental Learnability", 
        main = "Similarity-Based Social Learning", ylim = c(0.09,0.19))
lines(avy3, lwd = 3)

# Age based social learning
plot4 <- summProbabilities[61:80,]
avy4 <- colMeans(plot4)

matplot(t(plot4), type = "l", xlab = "Timesteps", ylab = "Environmental Learnability", 
        main = "Age-Based Social Learning", ylim = c(0.09,0.19))
lines(avy4, lwd = 3)

# Conformity social learning
plot5 <- summProbabilities[81:100,]
avy5 <- colMeans(plot5)

matplot(t(plot5), type = "l", xlab = "Timesteps", ylab = "Environmental Learnability", 
        main = "Conformity Social Learning", ylim = c(0.09,0.19))
lines(avy5, lwd = 3)

# Plotting mean traits in the environment
randomMean <- colMeans(summMeanTraitsInSystem[1:20,])
payoffMean <- colMeans(summMeanTraitsInSystem[21:40,])
similarityMean <- colMeans(summMeanTraitsInSystem[41:60,])
ageMean <- colMeans(summMeanTraitsInSystem[61:80,])
conformityMean <- colMeans(summMeanTraitsInSystem[81:100,])

plot(randomMean, type = "l", col = "red", xlab = "Timesteps", ylab = "Mean Traits in the Environment",
     main = "Mean Traits in the Environment for Each SLS", ylim = c(0.35,0.52))
lines(payoffMean, col = "blue")
lines(similarityMean, col = "purple")
lines(ageMean, col = "green")
lines(conformityMean, col = "orange")
legend("topright", legend = c("Random", "Payoff", "Similarity", "Age", "Conformity"),
       col = c("red", "blue", "purple", "green", "orange"), lwd = 2, cex = 0.8)

# Comparing agent's age and the number of traits they have
AgeTraits <- rbind(Age = popAge, N_Traits = rowSums(popn))

# Mean traits in the branches
data <- readRDS("MeanTraitsInBranch.RData")

dev.new()
par(mfrow = c(2,3))
plot(data[[1]][,1], type = "l", col = "red", xlab = "Timesteps", ylab = "Mean Traits in a Branch",
     main = "Mean Traits in the Branches for bf = 4 (Simulation 1)", ylim = c(21,60))
lines(data[[1]][,2], type = "l", col = "blue")
lines(data[[1]][,3], type = "l", col = "green")
lines(data[[1]][,4], type = "l", col = "magenta")

plot(data[[2]][,1], type = "l", col = "red", xlab = "Timesteps", ylab = "Mean Traits in a Branch",
     main = "Mean Traits in the Branches for bf = 4 (Simulation 2)", ylim = c(21,60))
lines(data[[2]][,2], type = "l", col = "blue")
lines(data[[2]][,3], type = "l", col = "green")
lines(data[[2]][,4], type = "l", col = "magenta")

plot(data[[3]][,1], type = "l", col = "red", xlab = "Timesteps", ylab = "Mean Traits in a Branch",
     main = "Mean Traits in the Branches for bf = 4 (Simulation 3)", ylim = c(21,60))
lines(data[[3]][,2], type = "l", col = "blue")
lines(data[[3]][,3], type = "l", col = "green")
lines(data[[3]][,4], type = "l", col = "magenta")

plot(data[[4]][,1], type = "l", col = "red", xlab = "Timesteps", ylab = "Mean Traits in a Branch",
     main = "Mean Traits in the Branches for bf = 4 (Simulation 4)", ylim = c(21,60))
lines(data[[4]][,2], type = "l", col = "blue")
lines(data[[4]][,3], type = "l", col = "green")
lines(data[[4]][,4], type = "l", col = "magenta")

plot(data[[5]][,1], type = "l", col = "red", xlab = "Timesteps", ylab = "Mean Traits in a Branch",
     main = "Mean Traits in the Branches for bf = 4 (Simulation 5)", ylim = c(21,60))
lines(data[[5]][,2], type = "l", col = "blue")
lines(data[[5]][,3], type = "l", col = "green")
lines(data[[5]][,4], type = "l", col = "magenta")

# Variance in mean traits in the branches
variance <- read.csv("VarianceAcrossBranch")
variance$X <- factor(1:5)
colnames(variance) <- c("Simulation", paste("T", 1:20000, sep = ""))
varLong <- melt(variance, id.vars = "Simulation", variable.name = "Variable", value.name = "Value")

ggplot(varLong, aes(x = Variable, y = Value, group = Simulation, color = Simulation)) +
  geom_line() +
  theme_minimal() + 
  labs(title = "Variance in Mean Traits Across the Branches",
       x = "Timesteps",
       y = "Variance",
       color = "Simulation")

## 21-05-24 Simulation Results ##

# Mean traits in each branch
dev.new()
par(mfrow = c(2,3))
plot(summMeanTraitsInBranch[[1]][,1], type = "l", col = "red", ylab = "Mean Traits in Each Branch",
     xlab = "Timesteps", main = "Random", ylim = c(17,53))
lines(summMeanTraitsInBranch[[1]][,2], type = "l", col = "blue")
lines(summMeanTraitsInBranch[[1]][,3], type = "l", col = "green")
lines(summMeanTraitsInBranch[[1]][,4], type = "l", col = "magenta")

plot(summMeanTraitsInBranch[[2]][,1], type = "l", col = "red", ylab = "Mean Traits in Each Branch",
     xlab = "Timesteps", main = "Payoff", ylim = c(17,53))
lines(summMeanTraitsInBranch[[2]][,2], type = "l", col = "blue")
lines(summMeanTraitsInBranch[[2]][,3], type = "l", col = "green")
lines(summMeanTraitsInBranch[[2]][,4], type = "l", col = "magenta")

plot(summMeanTraitsInBranch[[3]][,1], type = "l", col = "red", ylab = "Mean Traits in Each Branch",
     xlab = "Timesteps", main = "Similarity", ylim = c(17,53))
lines(summMeanTraitsInBranch[[3]][,2], type = "l", col = "blue")
lines(summMeanTraitsInBranch[[3]][,3], type = "l", col = "green")
lines(summMeanTraitsInBranch[[3]][,4], type = "l", col = "magenta")

plot(summMeanTraitsInBranch[[4]][,1], type = "l", col = "red", ylab = "Mean Traits in Each Branch",
     xlab = "Timesteps", main = "Age", ylim = c(17,53))
lines(summMeanTraitsInBranch[[4]][,2], type = "l", col = "blue")
lines(summMeanTraitsInBranch[[4]][,3], type = "l", col = "green")
lines(summMeanTraitsInBranch[[4]][,4], type = "l", col = "magenta")

plot(summMeanTraitsInBranch[[5]][,1], type = "l", col = "red", ylab = "Mean Traits in Each Branch",
     xlab = "Timesteps", main = "Conformity", ylim = c(17,53))
lines(summMeanTraitsInBranch[[5]][,2], type = "l", col = "blue")
lines(summMeanTraitsInBranch[[5]][,3], type = "l", col = "green")
lines(summMeanTraitsInBranch[[5]][,4], type = "l", col = "magenta")

# Variance
variance <- read.csv("VarianceAcrossBranch")
variance$X <- factor(1:5)
colnames(variance) <- c("Simulation", paste("T", 1:20000, sep = ""))
varLong <- melt(variance, id.vars = "Simulation", variable.name = "Variable", value.name = "Value")

ggplot(varLong, aes(x = Variable, y = Value, group = Simulation, color = Simulation)) +
  geom_line() +
  theme_minimal() + 
  labs(title = "Variance in Mean Traits Across the Branches",
       x = "Timesteps",
       y = "Variance",
       color = "Simulation")

# Environmental learnability


# 7. OLD CODE

# Create matrix to track number of individuals with each trait (initial starting frequencies)
# traitSums <- colSums(popn)
# traitTracking <- matrix(traitSums[-1], nrow = branching_factor, ncol = (num_nodes - 1)/branching_factor)
# Trait labels (identifies which nodes are in which arms)
# traitDiagram <- matrix(2:num_nodes, nrow = branching_factor, ncol = (num_nodes - 1)/branching_factor) 

# Trait frequencies after the simulation
# finaltraitSums <- colSums(popn)
# finaltraitTracking <- matrix(finaltraitSums[-1], nrow = branching_factor, ncol = (num_nodes - 1)/branching_factor)

# Plotting frequency of traits per branch AFTER social learning
# traitPlot <- t(as.data.frame(finaltraitTracking))
# matplot(1:nrow(traitPlot), traitPlot[,1:ncol(traitPlot)], type = "l", lty = 1, col = 1:nrow(traitPlot), 
#         xlab = "Trait Depth of Branch", ylab = "Frequency", main = "Age-Based, bf = 4, After", ylim = c(0,100))

# Plotting frequency of traits per branch BEFORE social learning
# traitTracking <- t(as.data.frame(traitTracking))
# matplot(1:nrow(traitTracking), traitTracking[,1:ncol(traitTracking)], type = "l", lty = 1, col = 1:nrow(traitTracking), 
#         xlab = "Trait Depth of Branch", ylab = "Frequency", main = "Age-Based, bf = 4, Before", ylim = c(0,100))
