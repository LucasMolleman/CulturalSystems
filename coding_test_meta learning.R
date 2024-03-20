library(tidyverse)

## functions
#check if teacher has higher skill level
select_teacher <- function(skillset, teachers, skills_learner){
  skilled_teacher <- c() 
  for (i in 1:length(teachers)) {
    skills_teacher <- sum(skillset[,teachers[i]])
    cat("teacher:", teachers[i])
    print(skills_teacher)
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
  } 
}
payoff_based <- function(){
  teacher_highest_payoff <- c(skilled_teacher[skills_steacher == max(skills_steacher)])
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
  overview[individual, "Number_skills"] <- overview[individual, "Number_skills"]+1
  skillset[skills_learner + 1,individual] <- 1
  overview[individual, "Successful"] <<- overview[individual, "Successful"] +1
  cat(overview[individual,"Learning_strat"], "Successful")
  return(overview)
}
unsuccessful <- function(individual = individual, skillset = skillset, overview = overview){
  overview[individual, "Unsuccessful"] <<- overview[individual, "Unsuccessful"] +1
  cat(overview[individual,"Learning_strat"], "Unsuccessful")
  return(overview)
}
strategy_for_life <- function (learningstrat){
    if (learningstrat == 1){
      payoff_based()
      observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
      if(observed_behavior == skills_learner+1){
        successful(individual, skillset, overview)
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] + 1
      } else {
        unsuccessful(individual, skillset, overview)
        meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] + 1
      } }else if(learningstrat == 2){
        similarity_based()
        observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
        if(observed_behavior == skills_learner+1){
          successful(individual, skillset, overview)
          meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] + 1
        } else {
          unsuccessful(individual, skillset, overview)
          meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] + 1
        }
      }else if(learningstrat == 3){
        age_based()
        observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
        if(observed_behavior == skills_learner+1){
          successful(individual, skillset, overview)
          meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] + 1
        } else {
          unsuccessful(individual, skillset, overview)
          meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] + 1
        }
      } else if(learningstrat == 4){
        conformity_based()
        observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
        if(observed_behavior == skills_learner+1){
          successful(individual, skillset, overview)
          meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] + 1
        } else {
          unsuccessful(individual, skillset, overview)
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
    distr_bayesian <- rbeta(n = 1000, shape1 = 1 + a, shape2 = 1 + b)
    probs_learningstrat <- append(probs_learningstrat, sample(distr_bayesian, 1))
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
    distr_bayesian <- rbeta(n = 1000, shape1 = 1 + a, shape2 = 1 + b)
    probs_learningstrat <- append(probs_learningstrat, sample(distr_bayesian, 1))
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
    if(match(skilled_teacher[i], score_conformity) != NA){
      weighted_conformity_score[i] <- match(skilled_teacher[i], score_conformity) * probs_learningstrat[4]
      } else weighted_conformity_score[i] <- 1*probs_learningstrat[4]
    overall_score[i] <- weighted_payoff_score[i] + weighted_similiarity_score[i] + weighted_age_score[i] + weighted_conformity_score[i]
  }
  selected_teacher <- skilled_teacher[overall_score == max(overall_score)] 
  observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
  if(observed_behavior == skills_learner+1){
    successful(individual, skillset, overview)
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 4] + (weighted_payoff_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 4] + (weighted_similiarity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 4] + (weighted_age_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 4] + (weighted_conformity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
  } else {
    unsuccessful(individual, skillset, overview)
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 1, 5] + (weighted_payoff_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 2, 5] + (weighted_similiarity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 3, 5] + (weighted_age_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
    meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 5] <<- meta_overview[meta_overview$ID == individual & meta_overview$Learning_strat == 4, 5] + (weighted_conformity_score[overall_score == max(overall_score)]/overall_score[overall_score == max(overall_score)])
  }
}


meta_learning <- function(){
if (meta_overview[individual, 2] == 1) strategy_for_life(learningstrat = overview[individual,"Learning_strat"])
else if (meta_overview[individual, 2] == 2) {
    learningstrat <- bayesian_learner()
    strategy_for_life(learningstrat = learningstrat)
} else mixture_of_experts(meta_overview = meta_overview)
    }


###

# set up population
population <- c(seq(1:500))
skills <- 20
timesteps <- 300
rounds <- 30
# give them first subskill
skillset <- matrix(0, nrow = skills, ncol = length(population))
skillset[1,] <- 1

# learners who start with level > 1
advanced_learners <- sample(population, 100)
random_skill_level <- sample(skills, 100, replace = TRUE)
for(i in 1: length(advanced_learners)){
  x <- random_skill_level[i]
  while (x > 1) {
  skillset[x,advanced_learners[i]] <- 1
  x <- x-1
  }
}

# set ages
overview <- matrix(nrow = length(population), ncol = 5, dimnames = list(c(), c("Number_skills", "Age", "Learning_strat", "Successful", "Unsuccessful")))
overview[,"Age"] <- 1
overview[,"Successful"] <- 0
overview[,"Unsuccessful"] <- 0
for (i in 1: ncol(skillset)){
  overview[i, "Number_skills"] <- sum(skillset[,i])
}
# each individual gets one learning strat
overview[,"Learning_strat"] <- sample(1:4, length(population), replace = TRUE, prob = c(0.25, 0.25,0.25,0.25))

# build overview for meta strats
meta_overview <- matrix(nrow = length(population) * 4, ncol = 5, dimnames = list(c(), c("ID", "Meta_strategy", "Learning_strat", "Successful", "Unsuccessful")))
meta_overview[,1] <- rep(1:length(population),each = 4)
meta_strat <- sample(1:3, length(population), replace = TRUE, prob = c(1/3, 1/3, 1/3))
meta_overview[,2] <- rep(meta_strat, each = 4) 
meta_overview[,3] <- rep(1:4)
meta_overview[,4:5] <- 0
meta_overview <- as.data.frame(meta_overview)

# sample teacher 
for (i in 1: rounds) {
  for (t in 1:timesteps) {
  #browser()
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
  # have at least two skilled teachers to choose from
  while(new_teacher == TRUE){
    teachers <- sample(rest_pop, 10, replace = FALSE)
    select_teacher(skillset, teachers, skills_learner)
    if_no_teacher()
    if(length(skilled_teacher) > 1) new_teacher <- FALSE
  }
  # give them learning strat
  skills_steacher <- c(overview[skilled_teacher,"Number_skills"])
  prob_teacher_select <- rep(1/length(skilled_teacher), length(skilled_teacher))
  cat("round:", i, " ")
  meta_learning()
  overview[individual,2] <- overview[individual,2] + 1 
}
  }



## browser() to read it line by line

colSums(overview)
colSums(meta_overview)
colMeans(overview)

overview_dat <- as.data.frame(overview)
overview_dat <- overview_dat %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

meta_overview %>%
  group_by(Meta_strategy) %>%
  summarise_at(vars(3:4), list(name = mean))

ggplot(data = overview_dat, aes(x = Number_skills, fill = Learning_strat)) +
  geom_bar(position = position_dodge(width = 0.8))

meta_overview %>%
  filter(Meta_strategy == 3) 

