source("simFunctions.R")

runsimulation <- function(params, blockedLearningStrategy, repl, tree){ 
  ### define the cultural system ###
  ## Total payoffs are stored in column 1, payoffs for unblocked individuals in
  ## column 2, and payoffs for blocked individuals in column 3
  SLpay<-matrix(nrow=params$timesteps, ncol = 3)	

  # 		set payoffs for each trait
  attr(tree, "payoffs") <- getPayoffs(tree, params)
  ### SYSTEM AND NODE PAYOFFS ARE SET
  ####### INITIALIZE POPULATION #####
  blockers <- initializeBlockers(params, tree)
  blockedTraits <- which(colSums(blockers) > 0)
  blockedInds <- which(rowSums(blockers) > 0)
  tree <- addDetours(params, tree, blockedTraits, type = params$detourType)
  attr(tree, "requirements") <- get_requirements(tree) #add alternative routes around blocked traits
  attr(tree, "blockedTraits") <- blockedTraits
  repertoires<-initializePopulation(params, blockers, tree)
  if (ncol(repertoires) !=  igraph::gorder(tree)) {
    stop("Number of nodes in the tree does not match the number of nodes in the repertoires")
  }
  popAge <- assignAges(repertoires)
  failed_learning_count <- 0
  failed_learning_count_blocked <- 0
  tr_sums <- vector("list", params$timesteps)
  attr(tr_sums, "tree") <- tree
  tr_sums_blocked <- vector("list", params$timesteps)
  attr(tr_sums_blocked, "tree") <- tree
  
  failure_sums_blocked <- vector("list", params$timesteps)
  failures <- matrix(0, nrow = params$N, ncol = ncol(repertoires))
  
  payoff_IDs <- rep(NA, params$timesteps)
  
  ### population is now initialized... start running the model
  for (t in 1:params$timesteps){
    ind<-sample(1:params$N,1)
    payoff_IDs[[t]] <- ind
    if (ind %in% blockedInds){
      learningStrategy <- blockedLearningStrategy
    } else {
      learningStrategy <- blockedLearningStrategy
    }
    
    ## will they learn individually or socially?
    r<-runif(1)
    unknownTraits <- which(repertoires[ind,] == 0) 
    SLpay[t,]<-NA
    if (length(unknownTraits > 0)){  #only try to learn if there's anything to learn for this agent
      blockedTraits <- which(blockers[ind,] == 1)
      if (r<params$S) {  # social learning
        ## sample M random other individuals
        poolOthers <- setdiff(1:nrow(repertoires), ind) # agents do not sample themselves
          #models <- sample_age_biased(ind, popAge,params$age_bias, params$M)
        models <- sample(poolOthers, params$M)
        ## randomly pick 1 trait from each model
        ## only consider traits the learning agent does not know yet
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
        learning_result <- learnSocially(params,
                                         repertoires,
                                         blockers,
                                         ind,
                                         learningStrategy, 
                                         popAge,
                                         tree,
                                         observedTraits,
                                         observedModels,
                                         SLpay,
                                         payoff_IDs)

        learnedTrait <- learning_result$learned
        failed_trait <- learning_result$failed
        if (length(learnedTrait) == 1) {
        ######## calculate payoffs of learning
          repertoires[ind, learnedTrait] <- 1
          focalPay <- attributes(tree)$payoffs[learnedTrait]
          SLpay[t, 1] <- focalPay
          if (ind %in% blockedInds){
            SLpay[t, 3] <- focalPay
          } else {
            SLpay[t, 2] <- focalPay
          } 
        } else {
          SLpay[t,1] <- 0
          failed_learning_count <- failed_learning_count + 1
          failures[ind, failed_trait] <- failures[ind, failed_trait] + 1
          if (ind %in% blockedInds){
            failed_learning_count_blocked <- failed_learning_count_blocked + 1
            SLpay[t,3]<-0
          } else {
            SLpay[t,2]<-0
          }
        }
      }
      else if(r >= params$S){	# individual learning (=innovation)
        selectedTrait <- sample(unknownTraits,1)
        # Calculate learning probability
        pList <- unique(getTraitLearningProbability_R(repertoires, ind, attributes(tree)$requirements, selectedTrait))
        if (selectedTrait %in% blockedTraits) pList[1] <- pList[1] * 0.01
        if(length(pList) > 0){
          if (runif(1) < pList[1]){
            repertoires[ind,selectedTrait]<-1
          }
        }
      } 
      if (params$get_tr_sums) {
        tr_sums[[t]] <- colSums(repertoires[-blockedInds, ], na.rm =T)
        tr_sums_blocked[[t]] <- colSums(repertoires[blockedInds, ], na.rm =T)
        failure_sums_blocked[[t]] <- colSums(failures[blockedInds, ], na.rm =T)
      }
    }
    
    ## each time step the agent was sampled, their age increases by 1
    popAge[ind] <- popAge[ind] + 1
    
    ## replace an individual with a naive one at random
    ## NB this is not appropriate for evolutionary sims
    if (runif(1) < params$reset_rate) {
      repertoires[ind,]<-c(1,rep(0,ncol(repertoires) - 1))
      popAge[ind]<-0  ## reset the age of the agent to 0
      payoff_IDs[payoff_IDs == ind] <- NA #reset so agents don't use information from dead agents
    }
  }

  
  if (params$get_tr_sums) {
    for(i in 2:length(tr_sums_blocked)){
      if(is.null(tr_sums_blocked[[i]])) tr_sums_blocked[[i]] <- tr_sums_blocked[[i-1]]
      if(is.null(failure_sums_blocked[[i]])) failure_sums_blocked[[i]] <- failure_sums_blocked[[i-1]]
    }
    saveRDS(tr_sums, paste0(params$data_path,"tr_sums.rds"))
    saveRDS(tr_sums_blocked, paste0(params$data_path,"tr_sums_blocked.rds"))
    saveRDS(failure_sums_blocked, paste0(params$data_path,"failure_sums_blocked.rds"))
  }
  
  trim <- 1:params$timesteps
  sumThisSimulation<-c(igraph::gorder(tree), 
                        blockedLearningStrategy,
                        repl,
                        params$payoff_scaling,
                        params$blockedLayer,
                        params$numBlocked,
                        params$numSteps,
                        params$propBlocked,
                        mean(SLpay[trim,1], na.rm=TRUE),
                        mean(SLpay[trim,2], na.rm=TRUE),
                        mean(SLpay[trim,3], na.rm=TRUE),
                        failed_learning_count/params$timesteps,
                        failed_learning_count_blocked/sum(!is.na(SLpay[,3])),
                        params$age_bias,
                        params$tree_func)
  
  
  return(sumThisSimulation)
}


run_all_simulations_parallel <- function(iterations, params, tree) {
  if (nrow(iterations) > 1 & params$get_tr_sums) {
    warning("get_tr_sums is not supported when running multiple simulations\nsetting get_tr_sums to FALSE")
    params$get_tr_sums <- FALSE
  }
  
  print(paste("starting", nrow(iterations), "simulations in parallel..."))
  simulation_results <- furrr::future_pmap(
    iterations,
    function(learningStrategy, numSteps, blockedLayer, propBlocked, repl, age_bias, tree_func) {
      params$numSteps <- numSteps
      params$blockedLayer <- blockedLayer
      params$propBlocked <- propBlocked
      params$typical_learning_strategy <- learningStrategy
      params$age_bias <- age_bias
      params$tree_func <- tree_func
      if (tree_func == 0) {
        tree <- generate_flat_tree()
      } else {
        tree <- generate_rooted_tree()
      }
      run_result <- runsimulation(params, learningStrategy, repl, tree)
      return(run_result)
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  # Convert list of vectors to a matrix
  strategySuccess <- do.call(rbind, simulation_results)
  
  return(strategySuccess)
}


run_all_simulations <- function(iterations, params, tree) {
  if (nrow(iterations) > 1 & params$get_tr_sums) {
    warning("get_tr_sums is not supported when running multiple simulations\nsetting get_tr_sums to FALSE")
    params$get_tr_sums <- FALSE
  }
  
  print(paste("starting", nrow(iterations), "simulations..."))
  

  results <-  purrr::pmap(iterations, function(learningStrategy, numSteps, blockedLayer, propBlocked, repl, age_bias, tree_func) {
    params$numSteps <- numSteps
    params$blockedLayer <- blockedLayer
    params$propBlocked <- propBlocked
    params$typical_learning_strategy <- learningStrategy
    params$age_bias <- age_bias
    params$tree_func <- tree_func
    if (tree_func == 0) {
      tree <- generate_flat_tree()
    } else {
      tree <- generate_rooted_tree()
    }
    print(paste("Learning Strategy:", learningStrategy))
    sumThisSimulation <- runsimulation(params, learningStrategy, repl, tree)
  })
  do.call(rbind, results)
}

get_obj_seed <- function(object) {
  as.numeric(sapply(serialize(object, NULL), function(x) as.integer(x) %% .Machine$integer.max)) %>% sum() %% .Machine$integer.max
}



















