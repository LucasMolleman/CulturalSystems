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
learn_socially <- function (){
  if (length(skilled_teacher) > 1){
  if (overview[individual,"Learning_strat"] == 1){
    payoff_based()
  } else if(overview[individual,"Learning_strat"] == 2){
    similarity_based()
  }else if(overview[individual,"Learning_strat"] == 3){
    age_based()
  } else if(overview[individual,"Learning_strat"] == 4){
    conformity_based()
  }
  observed_behavior <- sample(seq(skills_learner+1, skills_steacher[skilled_teacher == selected_teacher]), 1)
  } else observed_behavior <- sample(seq(skills_learner+1, skills_steacher), 1)
  if(observed_behavior == skills_learner+1){
    overview[individual, "Number_skills"] <- overview[individual, "Number_skills"]+1
    skillset[skills_learner + 1,individual] <- 1
    overview[individual, "Successful"] <<- overview[individual, "Successful"] +1
    cat(overview[individual,"Learning_strat"], observed_behavior, "Successful")
  } else {
    overview[individual, "Unsuccessful"] <<- overview[individual, "Unsuccessful"] +1
    cat(overview[individual,"Learning_strat"], observed_behavior, "Unsuccessful")
  }
}


# set up population
population <- c(seq(1:500))
skills <- 20
rounds <- 100
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
overview <- matrix(nrow = length(population), ncol = 6, dimnames = list(c(), c("Number_skills", "Age", "Learning_strat", "Meta_strat", "Successful", "Unsuccessful")))
overview[,"Age"] <- 1
overview[,"Successful"] <- 0
overview[,"Unsuccessful"] <- 0
for (i in 1: ncol(skillset)){
  overview[i, "Number_skills"] <- sum(skillset[,i])
}
# each individual gets one learning strat
overview[,"Learning_strat"] <- sample(1:4, 1000, replace = TRUE, prob = c(0.25, 0.25,0.25,0.25))


# sample teacher 
for (i in 1: rounds) {
  individual <- sample(population, 1)
  skills_learner <- sum(skillset[,individual])
  if (skills_learner == 20){
    new_learner <- TRUE
    while(new_learner == TRUE){
      individual <- sample(population, 1)
      skills_learner <- sum(skillset[,individual])
      if (skills_learner == 20) new_learner <- FALSE
    }
  }
  rest_pop <- population[-individual]
  teachers <- sample(rest_pop, 10, replace = FALSE)
  select_teacher(skillset, teachers, skills_learner)
  if_no_teacher() 
  # give them learning strat
  skills_steacher <- c(overview[skilled_teacher,"Number_skills"])
  prob_teacher_select <- rep(1/length(skilled_teacher), length(skilled_teacher))
  cat("round:", i, " ")
  learn_socially()
}






  


