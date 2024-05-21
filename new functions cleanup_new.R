library(tidyverse)
library(hrbrthemes)
library(viridis)
library(stringr)
library(rjson)
library(forcats)
library(gganimate)
library(transformr)
library(readr)
library(bayesrules)
library(MARSS)
library(gdata)
library(gifski)


## functions
# select teacher
select_teacher <- function(teachers){
  skilled_teacher <- c() 
  skills_teacher <- c()
  for (i in 1:length(teachers)) {
    teacher <- teachers[i]
    skills_teacher[i] <- t_skills[teacher]
    if(skills_teacher[i] > t_skills[individual]){
      skilled_teacher <- append(skilled_teacher, teacher) 
    }
  }
  skilled_teacher <- skilled_teacher
}

# social learning strategies
payoff_based <- function(){
  payoff_teachers <- r_payoffs[t_skills[skilled_teacher]]
  teacher_highest_payoff <- c(skilled_teacher[payoff_teachers == max(payoff_teachers)])
  if (length(teacher_highest_payoff) > 1){
    teacher_highest_payoff <- sample(teacher_highest_payoff,1)
  }
  selected_teacher <- skilled_teacher[skilled_teacher == teacher_highest_payoff]
}
# similar traits
similarity_based <- function(){
  similarity_score <- c(t_skills[skilled_teacher] - t_skills[individual])
  similar_teacher <- c(skilled_teacher[similarity_score == min(similarity_score)])
  if (length(similar_teacher) > 1){
    similar_teacher <- sample(similar_teacher,1)
  }
  selected_teacher <- skilled_teacher[skilled_teacher == similar_teacher]
}
# similar age
age_based <- function(){
  teacher_age <- t_ages[skilled_teacher]
  learner_age <- t_ages[individual]
  age_difference <- c(teacher_age - learner_age)
  w <- c()
  for (i in 1:length(skilled_teacher)){
    if (age_difference[i] >= 0) w[i] <- 0.5^age_difference[i]
    if (age_difference[i] < 0) w[i] <- 10^-8
  }
  similar_age_teacher <- c(skilled_teacher[w == max(w)])
  # similar_age_teacher <- c(skilled_teacher[age_difference == min(age_difference)])
  if (length(similar_age_teacher) > 1){
    similar_age_teacher <- sample(similar_age_teacher,1)
  }
  selected_teacher <- skilled_teacher[skilled_teacher == similar_age_teacher]
}
# conformity, weigh common traits 
conformity_based <- function(){
  teacher_conf <- c()
  teacher_skills <- t_skills[skilled_teacher]
  for (i in 1: length(teacher_skills)){
    same <- length(which(teacher_skills==teacher_skills[i]))
    if (same > 1) {
      if(!(skilled_teacher[i] %in% teacher_conf)) {
        teacher_conf <- append(teacher_conf, skilled_teacher[teacher_skills == teacher_skills[i]])
      }
    }
  }
  if (length(teacher_conf) > 1){
    teacher_conf <- sample(teacher_conf,1)
    selected_teacher <- skilled_teacher[skilled_teacher == teacher_conf]
  }else if (length(teacher_conf) == 0){
    selected_teacher <- resample(skilled_teacher, 1)
  }
} 


# what does the learner do this round: reset, IL, or SL
state_learning <- function(){
  s <- runif(1)
  r <- runif(1)
  if(r <= reset_rate*t_ages[individual]) {
    state <- "reset"
  } else if (s < social_learning){
    state <- "SL"
  } else if (s >= social_learning) state <- "IL"
}

# IL 
practice_skills <- function(){
  practice_sucess <- sample(seq(1:10), 1)
  if (practice_sucess < 5){
    selected_trait <- skills-1
  } else selected_trait <- skills
  selected_trait
}

individual_learning <- function(){
  learnable_traits <- c((t_skills[individual] + 1) : skills)
  learnable_traits <- resample(learnable_traits,1)
  if (learnable_traits == (t_skills[individual] + 1)){
    selected_trait <- learnable_traits
  } else selected_trait <- t_skills[individual]
  selected_trait
}

# meta-strategies
fixed_strategy <- function (learningstrat){
  if (learningstrat == 1){
    selected_teacher <- payoff_based()
    skill_range <- (t_skills[individual]+1):t_skills[selected_teacher]
    observed_behavior <- resample(skill_range, 1)
  }else if(learningstrat == 2){
      selected_teacher <- similarity_based()
      skill_range <- (t_skills[individual]+1):t_skills[selected_teacher]
      observed_behavior <- resample(skill_range, 1)
    }else if(learningstrat == 3){
      selected_teacher <- age_based()
      skill_range <- (t_skills[individual]+1):t_skills[selected_teacher]
      observed_behavior <- resample(skill_range, 1)
    } else if(learningstrat == 4){
      selected_teacher <- conformity_based()
      skill_range <- (t_skills[individual]+1):t_skills[selected_teacher]
      observed_behavior <- resample(skill_range, 1)
    } 
  list(selected_teacher, observed_behavior)
}

flexible_strategy <- function(){
  successes <- c(t_strat1_success[individual],
                 t_strat2_success[individual],
                 t_strat3_success[individual], 
                 t_strat4_success[individual] 
  )
  failures <-  c(t_strat1_failure[individual],
                 t_strat2_failure[individual], 
                 t_strat3_failure[individual], 
                 t_strat4_failure[individual]
                 )                   
  probs_learningstrat <- c()
  for(learningstrat in 1:4){
    a <- successes[learningstrat]
    b <- failures[learningstrat]
    distr_bayesian <- rbeta(n = 1, shape1 = 1 + a, shape2 = 1 + b)
    probs_learningstrat <- append(probs_learningstrat, distr_bayesian)
    
  }
  list(probs_learningstrat, which.max(probs_learningstrat))
}
update_weights_success <- function(learningstrat){
  if (learningstrat == 1){
    strat1_success <- t_strat1_success[individual] + 1
    strat2_success <- t_strat2_success[individual] 
    strat3_success <- t_strat3_success[individual]
    strat4_success <- t_strat4_success[individual]
  }
  if (learningstrat == 2){
    strat1_success <- t_strat1_success[individual] 
    strat2_success <- t_strat2_success[individual] + 1
    strat3_success <- t_strat3_success[individual]
    strat4_success <- t_strat4_success[individual]
  }
  if (learningstrat == 3){
    strat1_success <- t_strat1_success[individual] 
    strat2_success <- t_strat2_success[individual] 
    strat3_success <- t_strat3_success[individual]+ 1
    strat4_success <- t_strat4_success[individual]
  }
  if (learningstrat == 4){
    strat1_success <- t_strat1_success[individual] 
    strat2_success <- t_strat2_success[individual] 
    strat3_success <- t_strat3_success[individual]
    strat4_success <- t_strat4_success[individual] + 1
  }
  list(strat1_success, strat2_success, strat3_success, strat4_success)
}
update_weights_failure <- function(learningstrat){
  if (learningstrat == 1){
    strat1_failure <- t_strat1_failure[individual] + 1
    strat2_failure <- t_strat2_failure[individual] 
    strat3_failure <- t_strat3_failure[individual]
    strat4_failure <- t_strat4_failure[individual]
  }
  if (learningstrat == 2){
    strat1_failure <- t_strat1_failure[individual] 
    strat2_failure <- t_strat2_failure[individual] + 1
    strat3_failure <- t_strat3_failure[individual]
    strat4_failure <- t_strat4_failure[individual]
  }
  if (learningstrat == 3){
    strat1_failure <- t_strat1_failure[individual] 
    strat2_failure <- t_strat2_failure[individual] 
    strat3_failure <- t_strat3_failure[individual]+ 1
    strat4_failure <- t_strat4_failure[individual]
  }
  if (learningstrat == 4){
    strat1_failure <- t_strat1_failure[individual] 
    strat2_failure <- t_strat2_failure[individual] 
    strat3_failure <- t_strat3_failure[individual]
    strat4_failure <- t_strat4_failure[individual] + 1
  }
  list(strat1_failure, strat2_failure, strat3_failure, strat4_failure)
}

calculate_z_score <- function(vec){
  if (length(unique(vec)) == 1){
    score <- rep(0, length(vec))
  } else score <- zscore(vec)
  return(score)
}

integrative_strategy <- function(){
  # sort teachers by fit for each learningstrat
  skills_teacher <- t_skills[skilled_teacher]
  skills_learner <- t_skills[individual]
  teacher_payoff <- r_payoffs[t_skills[skilled_teacher]]
  score_payoff <- calculate_z_score(teacher_payoff)
  #score_payoff <- skilled_teacher[order(teacher_payoff, decreasing = FALSE)]
  similarity_score <- c(skills_teacher - skills_learner)
  score_similarity <- calculate_z_score(similarity_score)
  #score_similarity <- skilled_teacher[order(similarity_score, decreasing = TRUE)]
  teacher_age <- c(t_ages[skilled_teacher])
  learner_age <- t_ages[individual]
  age_difference <- c(abs(teacher_age - learner_age))
  score_age <- calculate_z_score(age_difference)
  #score_age <- skilled_teacher[order(age_difference, decreasing = TRUE)]
  teacher_conf <- c()
  for (i in 1: length(skills_teacher)){
    teacher_conf[i] <- length(which(skills_teacher==skills_teacher[i])) - 1
  }
  score_conf <- calculate_z_score(teacher_conf)
  # find out how well each learning strat performed before for the individual
  successes <- c(t_strat1_success[individual],
                  t_strat2_success[individual],
                  t_strat3_success[individual], 
                  t_strat4_success[individual] 
  )
  failures <-  c(t_strat1_failure[individual],
                 t_strat2_failure[individual], 
                 t_strat3_failure[individual], 
                 t_strat4_failure[individual]
  )                   
  probs_learningstrat <- c()
  for(learningstrat in 1:4){
    a <- successes[learningstrat]
    b <- failures[learningstrat]
    distr_bayesian <- rbeta(n = 1, shape1 = 1 + a, shape2 = 1 + b)
    probs_learningstrat <- append(probs_learningstrat, distr_bayesian)
    }
  weighted_payoff_score <- c()
  weighted_similiarity_score <- c()
  weighted_age_score <- c()
  weighted_conformity_score <- c()
  overall_score <- c()
  for (i in 1:length(skilled_teacher)){
    weighted_payoff_score[i] <- score_payoff[i] * probs_learningstrat[1]
    weighted_similiarity_score[i] <- score_similarity[i] * probs_learningstrat[2]
    weighted_age_score[i] <- score_age[i] * probs_learningstrat[3]
    weighted_conformity_score[i] <- score_conf[i] * probs_learningstrat[4]
    overall_score[i] <- weighted_payoff_score[i] - weighted_similiarity_score[i] - weighted_age_score[i] + weighted_conformity_score[i] ## was + for all
  }
  selected_teacher <- resample(skilled_teacher[overall_score == max(overall_score)], 1)
  skill_range <- (t_skills[individual]+1):t_skills[selected_teacher]
  observed_behavior <- resample(skill_range, 1)
  list(selected_teacher, 
       observed_behavior, 
       overall_score[skilled_teacher == selected_teacher], 
       weighted_payoff_score[skilled_teacher == selected_teacher], 
       weighted_similiarity_score[skilled_teacher == selected_teacher], 
       weighted_age_score[skilled_teacher == selected_teacher], 
       weighted_conformity_score[skilled_teacher == selected_teacher])
}
update_integrative_success <- function(){
  abs_total <- abs(weights_payoff) + abs(weights_similarity)+ abs(weights_age) + abs(weights_conformity) + 10^-12
  strat1_success <- t_strat1_success[individual] + (abs(weights_payoff)/abs_total)
  strat2_success <- t_strat2_success[individual] + (abs(weights_similarity)/abs_total)
  strat3_success <- t_strat3_success[individual] + (abs(weights_age)/abs_total)
  strat4_success <- t_strat4_success[individual] + (abs(weights_conformity)/abs_total)
  list(strat1_success, strat2_success, strat3_success, strat4_success)
}
update_integrative_failure <- function(){
  abs_total <- abs(weights_payoff) + abs(weights_similarity)+ abs(weights_age) + abs(weights_conformity)+ 10^-12
  strat1_failure <- t_strat1_failure[individual] + (abs(weights_payoff)/abs_total)
  strat2_failure <- t_strat2_failure[individual] + (abs(weights_similarity)/abs_total)
  strat3_failure <- t_strat3_failure[individual] + (abs(weights_age)/abs_total)
  strat4_failure <- t_strat4_failure[individual] + (abs(weights_conformity)/abs_total)
  list(strat1_failure, strat2_failure, strat3_failure, strat4_failure)
}
