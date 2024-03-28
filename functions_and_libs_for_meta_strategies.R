library(tidyverse)
library(hrbrthemes)
library(viridis)

## functions
#check if teacher has higher skill level
select_teacher <- function(skillset, teachers, skills_learner){
  skilled_teacher <- c() 
  for (i in 1:length(teachers)) {
    skills_teacher <- sum(skillset[,teachers[i]])
    if(skills_teacher > skills_learner){
      skilled_teacher <- append(skilled_teacher, teachers[i]) 
    }
  }
  skilled_teacher <<- skilled_teacher
  print(skilled_teacher)
}
if_no_teacher <- function(){
  while (is.null(skilled_teacher) == TRUE) {
    teachers <<- sample(rest_pop, 10, replace = FALSE)
    select_teacher(skillset, teachers, skills_learner)
    print(sum(skillset[,skilled_teacher]))
    k <<- k+1 
    if (k > 10) {
      individual <- sample(population, 1)
      skills_learner <- sum(skillset[,individual])
      if (skills_learner == 20){
        new_learner <- TRUE
        while(new_learner == TRUE){
          individual <- sample(population, 1)
          skills_learner <- sum(skillset[,individual])
          if (skills_learner < 20) new_learner <- FALSE
        }
      }
      rest_pop <- population[-individual]
      new_teacher <- TRUE
      
    }
  } 
}
payoff_based <- function(){
  payoff_teachers <- payoffs[skills_steacher]
  teacher_highest_payoff <- c(skilled_teacher[payoff_teachers == max(payoff_teachers)])
  if (length(teacher_highest_payoff) > 1){
    teacher_highest_payoff <- sample(teacher_highest_payoff,1)
  }
  prob_teacher_select[skilled_teacher == teacher_highest_payoff] <- prob_teacher_select[skilled_teacher == teacher_highest_payoff] * 3
  selected_teacher <<- sample(skilled_teacher, 1, prob = prob_teacher_select)
}
# similar traits
similarity_based <- function(){
  similarity_score <- c(skills_steacher - skills_learner)
  similar_teacher <- c(skilled_teacher[similarity_score == min(similarity_score)])
  if (length(similar_teacher) > 1){
    similar_teacher <- sample(similar_teacher,1)
  }
  prob_teacher_select[skilled_teacher == similar_teacher] <- prob_teacher_select[skilled_teacher == similar_teacher] * 3
  selected_teacher <<- sample(skilled_teacher, 1, prob = prob_teacher_select)
}
# similar age
age_based <- function(){
  teacher_age <- c(overview[skilled_teacher,"Age"])
  learner_age <- overview[individual, "Age"]
  age_difference <- c(teacher_age - learner_age)
  similar_age_teacher <- c(skilled_teacher[age_difference == min(age_difference)])
  if (length(similar_age_teacher) > 1){
    similar_age_teacher <- sample(similar_age_teacher,1)
  }
  prob_teacher_select[skilled_teacher == similar_age_teacher] <- prob_teacher_select[skilled_teacher == similar_age_teacher] * 3
  selected_teacher <<- sample(skilled_teacher, 1, prob = prob_teacher_select)
}
# conformity, weigh common traits 
conformity_based <- function(){
  teacher_conf <- c()
  for (i in 1: length(skills_steacher)){
    same <- length(which(skills_steacher==skills_steacher[i]))
    if (same > 1) {
      if(!(skilled_teacher[i] %in% teacher_conf)) {
        teacher_conf <- append(teacher_conf, skilled_teacher[skills_steacher == skills_steacher[i]])
      }
    }
  }
  if (length(teacher_conf) > 1){
    teacher_conf <- sample(teacher_conf,1)
    prob_teacher_select[skilled_teacher == teacher_conf] <- prob_teacher_select[skilled_teacher == teacher_conf] * 3
    selected_teacher <<- sample(skilled_teacher, 1, prob = prob_teacher_select)
  }else if (length(teacher_conf) == 0){
    prob_teacher_select <- prob_teacher_select
    selected_teacher <<- sample(skilled_teacher, 1, prob = prob_teacher_select)
  }
} 

successful <- function(individual = individual, skillset = skillset, overview = overview){
  overview[individual, "Number_skills"] <<- overview[individual, "Number_skills"]+1
  overview[individual,10] <<- overview[individual,10] + payoffs[skills_learner + 1]
  skillset[skills_learner + 1,individual] <<- 1
  overview[individual, "Successful"] <<- overview[individual, "Successful"] +1
  return(overview)
}
unsuccessful <- function(individual = individual, skillset = skillset, overview = overview){
  overview[individual, "Unsuccessful"] <<- overview[individual, "Unsuccessful"] +1
  return(overview)
}
strategy_for_life <- function (learningstrat){
  if (learningstrat == 1){
    payoff_based()
    observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
    if(observed_behavior == skills_learner+1){
      successful(individual, skillset, overview)
      cat("Learning Strategy: 1 ", "Successful")
      meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] + 1
    } else {
      unsuccessful(individual, skillset, overview)
      cat("Learning Strategy: 1 ", "Unsuccessful")
      meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] + 1
    } }else if(learningstrat == 2){
      similarity_based()
      observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
      if(observed_behavior == skills_learner+1){
        successful(individual, skillset, overview)
        cat("Learning Strategy: 2 ", "Successful")
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] + 1
      } else {
        unsuccessful(individual, skillset, overview)
        cat("Learning Strategy: 2 ", "Unsuccessful")
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] + 1
      }
    }else if(learningstrat == 3){
      age_based()
      observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
      if(observed_behavior == skills_learner+1){
        successful(individual, skillset, overview)
        cat("Learning Strategy: 3 ", "Successful")
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] + 1
      } else {
        unsuccessful(individual, skillset, overview)
        cat("Learning Strategy: 3 ", "Unsuccessful")
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] + 1
      }
    } else if(learningstrat == 4){
      conformity_based()
      observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
      if(observed_behavior == skills_learner+1){
        successful(individual, skillset, overview)
        cat("Learning Strategy: 4 ", "Successful")
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] + 1
      } else {
        unsuccessful(individual, skillset, overview)
        cat("Learning Strategy: 4 ", "Unsuccessful")
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 5] + 1
      }
      
    } 
  return(overview)
  return(meta_overview)
}
bayesian_learner <- function(){
  successes_l <- c(meta_overview[meta_overview$ID == individual, 4:5])
  probs_learningstrat <- c()
  for(learningstrat in 1:4){
    a <- successes_l$Successful[learningstrat]
    b <- successes_l$Unsuccessful[learningstrat]
    # distr_bayesian <- rbeta(n = 10, shape1 = 1 + a, shape2 = 1 + b)
    #probs_learningstrat <- append(probs_learningstrat, sample(distr_bayesian, 1))
    mean_bayesian <- mean(rbeta(n = 100, shape1 = 1 + a, shape2 = 1 + b))
    probs_learningstrat <- append(probs_learningstrat, mean_bayesian)
    print(probs_learningstrat)
  }
  which.max(probs_learningstrat)
}
mixture_of_experts <- function(meta_overview = meta_overview){
  # sort teachers by fit for each learningstrat
  score_payoff <- skilled_teacher[order(skills_steacher, decreasing = FALSE)]
  similarity_score <- c(skills_steacher - skills_learner)
  score_similarity <- skilled_teacher[order(similarity_score, decreasing = TRUE)]
  teacher_age <- c(overview[skilled_teacher,"Age"])
  learner_age <- overview[individual, "Age"]
  age_difference <- c(abs(teacher_age - learner_age))
  score_age <- skilled_teacher[order(age_difference, decreasing = TRUE)]
  teacher_conf <- c()
  for (i in 1: length(skills_steacher)){
    same <- length(which(skills_steacher==skills_steacher[i]))
    if (same > 1) {
      if(!(skilled_teacher[i] %in% teacher_conf)) {
        teacher_conf <- append(teacher_conf, skilled_teacher[skills_steacher == skills_steacher[i]])
      }
    }
  }
  if (length(teacher_conf) > 1) {
    score_conformity <- skilled_teacher[order(teacher_conf, decreasing = FALSE)]
  } else score_conformity <- c(rep(1, length(skilled_teacher)))
  # find out how well each learning strat performed before for the individual
  successes_l <- c(meta_overview[meta_overview$ID == individual, 4:5])
  probs_learningstrat <- c()
  for(learningstrat in 1:4){
    a <- successes_l$Successful[learningstrat]
    b <- successes_l$Unsuccessful[learningstrat]
    # distr_bayesian <- rbeta(n = 10, shape1 = 1 + a, shape2 = 1 + b)
    # probs_learningstrat <- append(probs_learningstrat, sample(distr_bayesian, 1))
    mean_bayesian <- mean(rbeta(n = 100, shape1 = 1 + a, shape2 = 1 + b))
    probs_learningstrat <- append(probs_learningstrat, mean_bayesian)
  }
  weighted_payoff_score <- c()
  weighted_similiarity_score <- c()
  weighted_age_score <- c()
  weighted_conformity_score <- c()
  overall_score <- c()
  for (i in 1:length(skilled_teacher)){
    weighted_payoff_score[i] <- (1*match(skilled_teacher[i], score_payoff)) * probs_learningstrat[1]
    weighted_similiarity_score[i] <- (1*match(skilled_teacher[i], score_similarity)) * probs_learningstrat[2]
    weighted_age_score[i] <- (1*match(skilled_teacher[i], score_age)) * probs_learningstrat[3]
    if(sum(score_conformity) == 1*length(skilled_teacher)){
      weighted_conformity_score[i] <- probs_learningstrat[4]
    } else {
      if(!(skilled_teacher[i] %in% score_conformity)){
        score_conformity <- append(score_conformity, skilled_teacher[!(skilled_teacher[i] %in% score_conformity)], after = 0)
      }
      weighted_conformity_score[i] <- match(skilled_teacher[i], score_conformity) * probs_learningstrat[4]
    }
    overall_score[i] <- weighted_payoff_score[i] + weighted_similiarity_score[i] + weighted_age_score[i] + weighted_conformity_score[i]
  }
  selected_teacher <- skilled_teacher[overall_score == max(overall_score)] 
  observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
  if(observed_behavior == skills_learner+1){
    successful(individual, skillset, overview)
    print("Successful")
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] + (weighted_payoff_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] + (weighted_similiarity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] + (weighted_age_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] + (weighted_conformity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
  } else {
    unsuccessful(individual, skillset, overview)
    print("Unsuccessful")
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] + (weighted_payoff_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] + (weighted_similiarity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] + (weighted_age_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 5] + (weighted_conformity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
  }
}
meta_learning <- function(){
  s <- runif(1)
  if (runif(1) >= reset_rate){
    if (s < social_learning){
      print("no reset")
      if (sum(meta_overview[meta_overview$ID == individual, 2]) == 4) {
        print("Meta Strategy 1")
        strategy_for_life(learningstrat = overview[individual,"Learning_strat"])
        overview[individual,2] <<- overview[individual,2] + 1
      } 
      if (sum(meta_overview[meta_overview$ID == individual, 2]) == 8) {
        print("Meta Strategy 2")
        learningstrat <- bayesian_learner()
        strategy_for_life(learningstrat = learningstrat)
        age_ind <- as.numeric(overview[individual,2])
        over_time <<- append(over_time, c(individual, age_ind, learningstrat))
        overview[individual,2] <<- overview[individual,2] + 1
      } 
      if(sum(meta_overview[meta_overview$ID == individual, 2]) == 12) {
        print("Meta Strategy 3")
        mixture_of_experts(meta_overview = meta_overview)
        overview[individual,2] <<- overview[individual,2] + 1
      }
    } else if (s >= social_learning){ # individual learning
      learnable_traits<- c(which(skillset[,individual] == 0))
      if (length(learnable_traits)>1) {
        selected_trait<-sample(learnable_traits,1)
      } else  selected_trait <- learnable_traits
      if (selected_trait == (sum(skillset[,individual])+1)){
        skillset[selected_trait, individual] <<- 1
        overview[individual, "Ind_Learning_success"] <<- overview[individual, "Ind_Learning_success"] + 1
        overview[individual,2] <<- overview[individual,2] + 1
        overview[individual, "Number_skills"] <- overview[individual, "Number_skills"]+1
      } else{
        overview[individual, "Ind_Learning_failure"] <<- overview[individual, "Ind_Learning_failure"] + 1
        overview[individual,2] <<- overview[individual,2] + 1
      }
    }
  } else {
    overview[individual, "Number_skills"] <<- 1
    skillset[2: skills_learner,individual] <<- 0
    overview[individual, "Successful"] <<- 0
    overview[individual, "Unsuccessful"] <<- 0
    overview[individual, "resets"] <<- overview[individual, "resets"] + 1
    meta_overview[meta_overview$ID == individual,4:5] <<- 0 
    print("reset")
  }
}
