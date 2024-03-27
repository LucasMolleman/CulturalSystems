library(tidyverse)
library(hrbrthemes)
library(viridis)

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
    # distr_bayesian <- rbeta(n = 10, shape1 = 1 + a, shape2 = 1 + b)
    #probs_learningstrat <- append(probs_learningstrat, sample(distr_bayesian, 1))
    mean_bayesian <- mean(rbeta(n = 100, shape1 = 1 + a, shape2 = 1 + b))
    probs_learningstrat <- append(probs_learningstrat, mean_bayesian)
    
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
r <- runif(1)
if (r >= reset_rate){
  if (sum(meta_overview[meta_overview$ID == individual, 2]) == 4) {
  strategy_for_life(learningstrat = overview[individual,"Learning_strat"])
    } 
  if (sum(meta_overview[meta_overview$ID == individual, 2]) == 8) {
    learningstrat <- bayesian_learner()
    strategy_for_life(learningstrat = learningstrat)
    age_ind <- as.numeric(overview[individual,2])
    over_time <<- append(over_time, c(individual, age_ind, learningstrat))
    } 
  if(sum(meta_overview[meta_overview$ID == individual, 2]) == 12) mixture_of_experts(meta_overview = meta_overview)
} else {
  overview[individual, "Number_skills"] <- 1
  skillset[2: skills_learner,individual] <- 0
  overview[individual, "Successful"] <- 0
  overview[individual, "Unsuccessful"] <- 0
  meta_overview[meta_overview$ID == individual,4:5] <- 0 
}
  }


###

# set up population
population <- c(seq(1:500))
skills <- 20
timesteps <- 25000
rounds <- 10
reset_rate<- 0.01	
# learning_rate <- 30

# sample teacher 
for (r in 1: rounds) {
  # give them first subskill
  skillset <- matrix(0, nrow = skills, ncol = length(population))
  for (i in 1:length(population)){
  number_skills <- round(sample(runif(10000, 1, 20), 1), 0) 
  skillset[1:number_skills, i] <- 1
  } 
  
  # # learners who start with level > 1
  # advanced_learners <- sample(population, (length(population)/10))
  # random_skill_level <- sample(skills, 100, replace = TRUE)
  # for(i in 1: length(advanced_learners)){
  #   x <- random_skill_level[i]
  #   while (x > 1) {
  #     skillset[x,advanced_learners[i]] <- 1
  #     x <- x-1
  #   }
  # }
  # set ages
  overview <- matrix(nrow = length(population), ncol = 5, dimnames = list(c(), c("Number_skills", "Age", "Learning_strat", "Successful", "Unsuccessful")))
  overview[,"Age"] <- 0
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
  
  # build overview over time
  number_bayesian_learners <- meta_overview %>%
    filter(Meta_strategy == 2) %>%
    select(ID) %>%
    unique()
  number_bayesian_learners <- number_bayesian_learners[,1]
  over_time <- c()
  for (t in 1:timesteps) {
  # browser()
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
  k <- 1
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
assign(paste0("skillset", r), skillset, envir = .GlobalEnv)
assign(paste0("overview", r), overview, envir = .GlobalEnv)
assign(paste0("meta_overview", r), meta_overview, envir = .GlobalEnv)
assign(paste0("over_time", r), over_time, envir = .GlobalEnv)
  }



## browser() to read it line by line

#### plotting etc. ------------------------------
colSums(overview1)
colSums(overview2)
colSums(overview3)
colSums(meta_overview3)

overview_dat1 <- as.data.frame(overview1)
overview_dat1 <- overview_dat1 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat1 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat2 <- as.data.frame(overview2)
overview_dat2 <- overview_dat2 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat2 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat3 <- as.data.frame(overview3)
overview_dat3 <- overview_dat3 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat3 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat4 <- as.data.frame(overview4)
overview_dat4 <- overview_dat4 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat4 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat5 <- as.data.frame(overview5)
overview_dat5 <- overview_dat5 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat5 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat6 <- as.data.frame(overview6)
overview_dat6 <- overview_dat6 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat6 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat7 <- as.data.frame(overview7)
overview_dat7 <- overview_dat7 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat7 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat8 <- as.data.frame(overview8)
overview_dat8 <- overview_dat8 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat8 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat9 <- as.data.frame(overview9)
overview_dat9 <- overview_dat9 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat9 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

overview_dat10 <- as.data.frame(overview10)
overview_dat10 <- overview_dat10 %>%
  mutate(Learning_strat = factor(Learning_strat))
overview_dat10 %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Number_skills), list(name = mean))

meta_overview1 %>%
  group_by(Meta_strategy) %>%
  summarise_at(vars(3:4), list(name = mean))
meta_overview2 %>%
  group_by(Meta_strategy) %>%
  summarise_at(vars(3:4), list(name = mean))
meta_overview3 %>%
  group_by(Meta_strategy) %>%
  summarise_at(vars(3:4), list(name = mean))

ggplot(data = overview_dat1, aes(x = Number_skills, fill = Learning_strat)) +
  geom_bar(position = position_dodge(width = 0.8))

# get number of skills per meta strat
ID_strat1_run1 <- meta_overview1 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique() 
ID_strat1_run2 <- meta_overview2 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run3 <- meta_overview3 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run4 <- meta_overview4 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run5 <- meta_overview5 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run6 <- meta_overview6 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run7 <- meta_overview7 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run8 <- meta_overview8 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run9 <- meta_overview9 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()
ID_strat1_run10 <- meta_overview10 %>%
  filter(Meta_strategy == 1) %>%
  select(ID) %>%
  unique()

ID_strat1_run1 <- c(ID_strat1_run1$ID)
ID_strat1_run2 <- c(ID_strat1_run2$ID)
ID_strat1_run3 <- c(ID_strat1_run3$ID)
ID_strat1_run4 <- c(ID_strat1_run4$ID)
ID_strat1_run5 <- c(ID_strat1_run5$ID)
ID_strat1_run6 <- c(ID_strat1_run6$ID)
ID_strat1_run7 <- c(ID_strat1_run7$ID)
ID_strat1_run8 <- c(ID_strat1_run8$ID)
ID_strat1_run9 <- c(ID_strat1_run9$ID)
ID_strat1_run10 <- c(ID_strat1_run10$ID)

values_meta1_run1 <- overview_dat1[ID_strat1_run1, "Number_skills"]
values_meta1_run2 <- overview_dat2[ID_strat1_run2, "Number_skills"]
values_meta1_run3 <- overview_dat3[ID_strat1_run3, "Number_skills"]
values_meta1_run4 <- overview_dat4[ID_strat1_run4, "Number_skills"]
values_meta1_run5 <- overview_dat5[ID_strat1_run5, "Number_skills"]
values_meta1_run6 <- overview_dat6[ID_strat1_run6, "Number_skills"]
values_meta1_run7 <- overview_dat7[ID_strat1_run7, "Number_skills"]
values_meta1_run8 <- overview_dat8[ID_strat1_run8, "Number_skills"]
values_meta1_run9 <- overview_dat9[ID_strat1_run9, "Number_skills"]
values_meta1_run10 <- overview_dat10[ID_strat1_run10, "Number_skills"]


values_meta1 <- c(values_meta1_run1, values_meta1_run2, values_meta1_run3, values_meta1_run4, values_meta1_run5,
                  values_meta1_run6, values_meta1_run7, values_meta1_run8, values_meta1_run9, values_meta1_run10)
mean_meta1 <- sum(mean(values_meta1_run1), mean(values_meta1_run2), mean(values_meta1_run3), mean(values_meta1_run4), mean(values_meta1_run5),
                  mean(values_meta1_run6), mean(values_meta1_run7), mean(values_meta1_run8), mean(values_meta1_run9), mean(values_meta1_run10)) / 10
var_meta1 <- sum(var(values_meta1_run1), var(values_meta1_run2), var(values_meta1_run3), var(values_meta1_run4), var(values_meta1_run5),
                 var(values_meta1_run6), var(values_meta1_run7), var(values_meta1_run8), var(values_meta1_run9), var(values_meta1_run10)) / 10

ID_strat2_run1 <- meta_overview1 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique() 
ID_strat2_run2 <- meta_overview2 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run3 <- meta_overview3 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run4 <- meta_overview4 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run5 <- meta_overview5 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run6 <- meta_overview6 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run7 <- meta_overview7 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run8 <- meta_overview8 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run9 <- meta_overview9 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()
ID_strat2_run10 <- meta_overview10 %>%
  filter(Meta_strategy == 2) %>%
  select(ID) %>%
  unique()

ID_strat2_run1 <- c(ID_strat2_run1$ID)
ID_strat2_run2 <- c(ID_strat2_run2$ID)
ID_strat2_run3 <- c(ID_strat2_run3$ID)
ID_strat2_run4 <- c(ID_strat2_run4$ID)
ID_strat2_run5 <- c(ID_strat2_run5$ID)
ID_strat2_run6 <- c(ID_strat2_run6$ID)
ID_strat2_run7 <- c(ID_strat2_run7$ID)
ID_strat2_run8 <- c(ID_strat2_run8$ID)
ID_strat2_run9 <- c(ID_strat2_run9$ID)
ID_strat2_run10 <- c(ID_strat2_run10$ID)

values_meta2_run1 <- overview_dat1[ID_strat2_run1, "Number_skills"]
values_meta2_run2 <- overview_dat2[ID_strat2_run2, "Number_skills"]
values_meta2_run3 <- overview_dat3[ID_strat2_run3, "Number_skills"]
values_meta2_run4 <- overview_dat4[ID_strat2_run4, "Number_skills"]
values_meta2_run5 <- overview_dat5[ID_strat2_run5, "Number_skills"]
values_meta2_run6 <- overview_dat6[ID_strat2_run6, "Number_skills"]
values_meta2_run7 <- overview_dat7[ID_strat2_run7, "Number_skills"]
values_meta2_run8 <- overview_dat8[ID_strat2_run8, "Number_skills"]
values_meta2_run9 <- overview_dat9[ID_strat2_run9, "Number_skills"]
values_meta2_run10 <- overview_dat10[ID_strat2_run10, "Number_skills"]


values_meta2 <- c(values_meta2_run1, values_meta2_run2, values_meta2_run3, values_meta2_run4, values_meta2_run5,
                  values_meta2_run6, values_meta2_run7, values_meta2_run8, values_meta2_run9, values_meta2_run10)
mean_meta2 <- sum(mean(values_meta2_run1), mean(values_meta2_run2), mean(values_meta2_run3), mean(values_meta2_run4), mean(values_meta2_run5),
                  mean(values_meta2_run6), mean(values_meta2_run7), mean(values_meta2_run8), mean(values_meta2_run9), mean(values_meta2_run10)) / 10
var_meta2 <- sum(var(values_meta2_run1), var(values_meta2_run2), var(values_meta2_run3), var(values_meta2_run4), var(values_meta2_run5),
                 var(values_meta2_run6), var(values_meta2_run7), var(values_meta2_run8), var(values_meta2_run9), var(values_meta2_run10)) / 10

ID_strat3_run1 <- meta_overview1 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique() 
ID_strat3_run2 <- meta_overview2 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run3 <- meta_overview3 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run4 <- meta_overview4 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run5 <- meta_overview5 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run6 <- meta_overview6 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run7 <- meta_overview7 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run8 <- meta_overview8 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run9 <- meta_overview9 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()
ID_strat3_run10 <- meta_overview10 %>%
  filter(Meta_strategy == 3) %>%
  select(ID) %>%
  unique()

ID_strat3_run1 <- c(ID_strat3_run1$ID)
ID_strat3_run2 <- c(ID_strat3_run2$ID)
ID_strat3_run3 <- c(ID_strat3_run3$ID)
ID_strat3_run4 <- c(ID_strat3_run4$ID)
ID_strat3_run5 <- c(ID_strat3_run5$ID)
ID_strat3_run6 <- c(ID_strat3_run6$ID)
ID_strat3_run7 <- c(ID_strat3_run7$ID)
ID_strat3_run8 <- c(ID_strat3_run8$ID)
ID_strat3_run9 <- c(ID_strat3_run9$ID)
ID_strat3_run10 <- c(ID_strat3_run10$ID)

values_meta3_run1 <- overview_dat1[ID_strat3_run1, "Number_skills"]
values_meta3_run2 <- overview_dat2[ID_strat3_run2, "Number_skills"]
values_meta3_run3 <- overview_dat3[ID_strat3_run2, "Number_skills"]
values_meta3_run4 <- overview_dat4[ID_strat3_run4, "Number_skills"]
values_meta3_run5 <- overview_dat5[ID_strat3_run5, "Number_skills"]
values_meta3_run6 <- overview_dat6[ID_strat3_run6, "Number_skills"]
values_meta3_run7 <- overview_dat7[ID_strat3_run7, "Number_skills"]
values_meta3_run8 <- overview_dat8[ID_strat3_run8, "Number_skills"]
values_meta3_run9 <- overview_dat9[ID_strat3_run9, "Number_skills"]
values_meta3_run10 <- overview_dat10[ID_strat3_run10, "Number_skills"]

values_meta3 <- c(values_meta3_run1, values_meta3_run2, values_meta3_run3, values_meta3_run4, values_meta3_run5,
                  values_meta3_run6, values_meta3_run7, values_meta3_run8, values_meta3_run9, values_meta3_run10)
mean_meta3 <- sum(mean(values_meta3_run1), mean(values_meta3_run2), mean(values_meta3_run3), mean(values_meta3_run4), mean(values_meta3_run5),
                  mean(values_meta3_run6), mean(values_meta3_run7), mean(values_meta3_run8), mean(values_meta3_run9), mean(values_meta3_run10)) / 10
var_meta3 <- sum(var(values_meta3_run1), var(values_meta3_run2), var(values_meta3_run3), var(values_meta3_run4), var(values_meta3_run5),
                 var(values_meta3_run6), var(values_meta3_run7), var(values_meta3_run8), var(values_meta3_run9), var(values_meta3_run10)) / 10

###

mean_values <- c(mean_meta1, mean_meta2, mean_meta3)
variance_values <- c(var_meta1, var_meta2, var_meta3)

# Create a boxplot
boxplot(values_meta1, values_meta2, values_meta3, names = c("Strat for life", "Bayesian Learner", "MoE"), 
        main = "Boxplot with Mean and Variance",
        ylab = "Number of skills")

# Add mean points to the plot
points(1:3, mean_values, col = "red", pch = 19)

# Add error bars representing variance
arrows(1:3, mean_values - sqrt(variance_values), 1:3, mean_values + sqrt(variance_values), 
       angle = 90, code = 3, length = 0.1, col = "blue")



### plot bayesian learner development over time
# plot the usage of learning strategies over time
time_overview <- as.data.frame(matrix(over_time2, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview_sorted <- time_overview %>%
  arrange(ID)

time_overview_examples <- time_overview_sorted[1:4000,]

ggplot(time_overview_examples, aes(x = Age, y = Strategy)) +
  geom_line() +
  labs(x = "Age", y = "Stratgy") +
  ggtitle("Learning strategy for each learning round by ID") +
  facet_wrap(~ ID, ncol = 8) 

# wider overview for each ID over time
time_overview_wider <- time_overview %>%
  pivot_wider(
  names_from = Age,
  values_from = Strategy
  ) %>%
  mutate(ID = as.factor(ID))

# plot count for each learning stratgey over time
ggplot(time_overview_sorted, aes(x = Age, y = Strategy)) +
  geom_line() +
  labs(x = "Age", y = "Stratgy") +
  ggtitle("Values for each round by ID") 

df_counts <- time_overview_sorted %>%
  group_by(Age) %>%
  summarize(Count_all = n())

df_time <- left_join(df_counts, time_overview_sorted) %>%
  group_by(Age, Strategy) %>%
  summarize(percent = n()/mean(Count_all))

# Plot count of each value for each round
ggplot(df_time, aes(x = Age, y = percent, color = as.factor(Strategy))) +
  geom_point() + 
  geom_line() + 
  theme_classic() +
  scale_color_discrete(name="Learning strategy")

# overview how much each learning strategy is used by the bayesian learners
meta_overview3 %>% 
  filter(Meta_strategy == 2) %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Successful, Unsuccessful), list(sum = sum))


meta_overview1 %>%
  filter(Meta_strategy == 3) %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(Successful, Unsuccessful), list(Mean = mean))
  
mean(overview_dat1$Number_skills)
min(overview_dat1$Number_skills)
median(overview_dat1$Number_skills)
plot(density(overview_dat1$Number_skills))


# use runif for everyone to skill up 
# define proportion learnable for each learning strat
# include fixed death rate 
# play around with payoffs
# animate pop skill level over time