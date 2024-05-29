source("new functions cleanup_new.R")
source("functions_flatTree.R")

# get current time in correct format for folder name
current_time <- Sys.time() %>% 
  str_replace_all(":", "-") %>%
  substr(1,19)

# create data path names
data_folder <- paste0("data/", current_time, "_flatTree")
sim_settings_path <- paste0(data_folder, "/sim_settings.json")
results_path <- paste0(data_folder, "/results.csv")
payoff_path <- paste0(data_folder, "/payoffs.csv")

# create data folder for sim settings and sim results
dir.create(data_folder)

# create json file with sim settings
# sim_dat <- fromJSON(file = "myJSON.json")
# population = sim_dat$population_size

###
# set up population
population <- 1:100
skills <- 40
timesteps <- 500
rounds <- 100
reset_rate <- 0.1	
social_learning <- 0.99
skill_probs <- c(rep(0.06, 24), rep(0.05, 6), rep(0.026, 10))

###
sim_settings <-list(population = length(population),
                    skills = skills,
                    timesteps = timesteps, 
                    rounds = rounds,
                    reset_rate = reset_rate,
                    social_learning = social_learning,
                    skill_probs = skill_probs 
)
write(toJSON(sim_settings), file = sim_settings_path)


# create lists
vec_timesteps <- c(rep(0, length(population)), 1:timesteps)
n_timestep <- rep(vec_timesteps, rounds) 
n_rounds <- c()
for (rnd in 1:rounds) {
  vec_rounds <- c(rep(rnd, length(population)), rep(rnd, timesteps))
  n_rounds <- append(n_rounds, vec_rounds)
}
n_skills <- c()
meta_strat <- c()
learning_strat <- c()
payoffs <- c()
learner_payoffs <- c()
ages <- c() 
strat1_success <- c()
strat1_failure <- c()
strat2_success <- c()
strat2_failure <- c()
strat3_success <- c()
strat3_failure <- c()
strat4_success <- c()
strat4_failure <- c()
teacher_ID <- c()
skill_attempted <- c()
SL_rate <- c()
SL_success <- c()
n_resets <- c()
IL_rate <- c()
IL_success <- c()
eligible_teachers <- c()
t_ID <- c()
ID <- c()


for (r in 1: rounds) {
  ID <- append(ID, population)
  t_ID <- c()
  # vectors to collect data from all rounds and 
  r_payoffs <- c()
  for (s in 1:skills){
    r_payoffs[s] <- runif(n = 1, min = 0.1, max = 1) * s
  }
  r_skills <- c()
  r_ages <- c()
  r_learning_strat <- c()
  r_meta_strat <- c()
  r_teacher_ID <- c()
  r_strat1_success <- c()
  r_strat1_failure <- c()
  r_strat2_success <- c()
  r_strat2_failure <- c()
  r_strat3_success <- c()
  r_strat3_failure <- c()
  r_strat4_success <- c()
  r_strat4_failure <- c()
  r_teacher_ID <- c()
  r_skill_attempted <- c()
  r_SL_rate <- c()
  r_SL_success<- c()
  r_n_resets <- c()
  r_IL_rate<- c()
  r_IL_success <- c()
  r_learner_payoffs <- c()
  r_eligible_teachers <- c()
  for (n in 1:length(population)){
    r_skills[n] <- sample(seq(1:skills), 1, prob = skill_probs)
    r_ages[n] <- r_skills[n]
    r_meta_strat[n] <- sample(1:3, 1, replace = TRUE, prob = c(1/3, 1/3, 1/3))
    if (r_meta_strat[n] == 1) {
      r_learning_strat[n] <- sample(1:4, 1, replace = TRUE)
    } else r_learning_strat[n] <- NA
    r_strat1_success[n] <- 0
    r_strat1_failure[n] <- 0
    r_strat2_success[n] <- 0
    r_strat2_failure[n] <- 0
    r_strat3_success[n] <- 0
    r_strat3_failure[n] <- 0
    r_strat4_success[n] <- 0
    r_strat4_failure[n] <- 0
    r_teacher_ID[n] <- NA
    r_skill_attempted[n] <- NA
    r_SL_rate[n] <- 0
    r_SL_success[n] <- NA
    r_n_resets[n] <- 0
    r_IL_rate[n] <- 0
    r_IL_success[n] <- NA
    r_learner_payoffs[n] <- r_payoffs[r_skills[n]]
    r_eligible_teachers[n] <- NA
  }
  
  # create lists to track for each time step
  t_skills <- r_skills
  t_ages <- r_ages
  t_learning_strat <- r_learning_strat
  t_strat1_success <- r_strat1_success
  t_strat1_failure <- r_strat1_failure
  t_strat2_success <- r_strat2_success
  t_strat2_failure <- r_strat2_failure
  t_strat3_success <- r_strat3_success
  t_strat3_failure <- r_strat3_failure
  t_strat4_success <- r_strat4_success
  t_strat4_failure <- r_strat4_failure
  t_teacherID <- r_teacher_ID
  t_skill_attempt <- r_skill_attempted
  t_SL_rate <- r_SL_rate
  t_SL_success <- r_SL_success
  t_resets <- r_n_resets
  t_IL_rate <- r_IL_rate
  t_IL_success <- r_IL_success
  t_learner_payoffs <- r_learner_payoffs
  t_eligible_teachers <- r_eligible_teachers
  for (t in 1:timesteps) {
    t_IL_success <- rep(NA, length(population))
    t_SL_success <- rep(NA, length(population))
    t_teacherID <- rep(NA, length(population))
    t_skill_attempt <- rep(NA, length(population))
    t_eligible_teachers <- rep(NA, length(population))
    for (n in 1:length(population)){ 
      if(r_meta_strat[n] == 2) {
        t_learning_strat[n] <- NA
      } else t_learning_strat[n] <- t_learning_strat[n]
    }
    # select learner
    individual <- sample(population, 1)
    if (t_skills[individual] == skills){
      new_learner <- TRUE
      while(new_learner == TRUE){
        individual <- sample(population, 1)
        if (t_skills[individual] < skills) new_learner <- FALSE
      }
    }
    # select teachers 
    rest_pop <- population[-individual]
    new_teacher <- TRUE
    # have at least one skilled teacher
    k <- 0
    while(new_teacher == TRUE){
      teachers <- sample(rest_pop, 10, replace = FALSE)
      skilled_teacher <- select_teacher(teachers)
      if (k > 9){
        individual <- sample(population, 1)
        if (t_skills[individual] == skills){
          new_learner <- TRUE
          while(new_learner == TRUE){
            individual <- sample(population, 1)
            if (t_skills[individual] < skills) new_learner <- FALSE
          }
        }
        # select teachers 
        rest_pop <- population[-individual]
        k <- 0
      } else {
        if(length(skilled_teacher) > 0) {
          new_teacher <- FALSE
        } else {
          teachers <- sample(rest_pop, 10, replace = FALSE)
          skilled_teacher <- select_teacher(teachers)
          k <- k+1
        }
      }
    }
    t_eligible_teachers[individual] <- length(skilled_teacher) 
    # select if learner is being reset, doing individual, or social learning 
    state <- state_learning()
    if(state == "reset"){
      t_skills[individual] <- 1
      t_ages[individual] <- 0
      t_strat1_success[individual] <- 0
      t_strat1_failure[individual] <- 0
      t_strat2_success[individual] <- 0
      t_strat2_failure[individual] <- 0
      t_strat3_success[individual] <- 0
      t_strat3_failure[individual] <- 0
      t_strat4_success[individual] <- 0
      t_strat4_failure[individual] <- 0
      t_resets[individual] <- t_resets[individual]+1
      t_learner_payoffs[individual] <- r_payoffs[1]
      t_IL_rate[individual] <- 0
      t_SL_rate[individual] <- 0
    }
    if(state == "IL"){ # individual learning 
      t_IL_rate[individual] <- t_IL_rate[individual]  + 1
      if(t_skills[individual] == skills) t_skills[individual] <- practice_skills()
      if (t_skills[individual] < skills){
        selected_trait <- individual_learning()
        if (selected_trait == t_skills[individual]){ # IL fail
          t_IL_success[individual] <- 0
        } else { # IL success
          t_IL_success[individual] <- 1
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skills[individual] + 1]
          t_skills[individual] <- selected_trait
        }
      }
    }
    if(state == "SL"){
      t_SL_rate[individual] <- t_SL_rate[individual] + 1
      if(r_meta_strat[individual] == 1){ # fixed meta-strategy
        learningstrat <- t_learning_strat[individual]
        select_teacher_behavior <- fixed_strategy(learningstrat)
        t_teacherID[individual] <- unlist(select_teacher_behavior[1])
        t_skill_attempt[individual] <- unlist(select_teacher_behavior[2])
        if(t_skill_attempt[individual] > t_skills[individual]){
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skill_attempt[individual]]
          t_skills[individual] <- t_skill_attempt[individual]
          t_SL_success[individual] <- 1
        } else t_SL_success[individual] <- 0
      }
      if(r_meta_strat[individual] == 2){ # flexible meta-strategy
        flex_learner <- flexible_strategy()
        strat_weights <- unlist(flex_learner[1])
        learningstrat <- unlist(flex_learner[2])
        t_learning_strat[individual] <- learningstrat
        select_teacher_behavior <- fixed_strategy(learningstrat)
        t_teacherID[individual] <- unlist(select_teacher_behavior[1])
        t_skill_attempt[individual] <- unlist(select_teacher_behavior[2])
        if(t_skill_attempt[individual] > t_skills[individual]){
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skill_attempt[individual]]
          t_skills[individual] <- t_skill_attempt[individual]
          t_SL_success[individual] <- 1
          new_weights <- update_weights_success(learningstrat)
          t_strat1_success[individual] <- unlist(new_weights[1])
          t_strat2_success[individual] <- unlist(new_weights[2])
          t_strat3_success[individual] <- unlist(new_weights[3])
          t_strat4_success[individual] <- unlist(new_weights[4])
          
        } else {
          t_SL_success[individual] <- 0
          new_weights <- update_weights_failure(learningstrat)
          t_strat1_failure[individual] <- unlist(new_weights[1])
          t_strat2_failure[individual] <- unlist(new_weights[2])
          t_strat3_failure[individual] <- unlist(new_weights[3])
          t_strat4_failure[individual] <- unlist(new_weights[4])
        }
      } 
      if(r_meta_strat[individual] == 3){
        integrative_learner <- integrative_strategy()
        t_teacherID[individual] <- unlist(integrative_learner[1])
        t_skill_attempt[individual] <- unlist(integrative_learner[2])
        teacher_score <- integrative_learner[3] %>%
          unlist() 
        weights_payoff <- unlist(integrative_learner[4])
        weights_similarity <- unlist(integrative_learner[5])
        weights_age <- unlist(integrative_learner[6])
        weights_conformity <- unlist(integrative_learner[7])
        if(t_skill_attempt[individual] > t_skills[individual]){
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skill_attempt[individual]]
          t_skills[individual] <- t_skill_attempt[individual]
          t_SL_success[individual] <- 1
          new_weights <- update_integrative_success()
          t_strat1_success[individual] <- unlist(new_weights[1])
          t_strat2_success[individual] <- unlist(new_weights[2])
          t_strat3_success[individual] <- unlist(new_weights[3])
          t_strat4_success[individual] <- unlist(new_weights[4])
          
        } else {
          t_SL_success[individual] <- 0
          new_weights <- update_integrative_failure()
          t_strat1_failure[individual] <- unlist(new_weights[1])
          t_strat2_failure[individual] <- unlist(new_weights[2])
          t_strat3_failure[individual] <- unlist(new_weights[3])
          t_strat4_failure[individual] <- unlist(new_weights[4])
        }
        
        
      }
    }  
    t_ages[individual] <- t_ages[individual]+1  
    # save results from this timestep
    t_ID <- append(t_ID, population[individual])
    r_skills <- append(r_skills, t_skills[individual])
    r_ages <- append(r_ages, t_ages[individual])
    r_meta_strat <- append(r_meta_strat, r_meta_strat[individual])
    r_learning_strat <- append(r_learning_strat, t_learning_strat[individual])
    r_strat1_success <- append(r_strat1_success, t_strat1_success[individual])
    r_strat2_success <- append(r_strat2_success, t_strat2_success[individual])
    r_strat3_success <- append(r_strat3_success, t_strat3_success[individual])
    r_strat4_success <- append(r_strat4_success, t_strat4_success[individual])
    r_strat1_failure <- append(r_strat1_failure, t_strat1_failure[individual])
    r_strat2_failure <- append(r_strat2_failure, t_strat2_failure[individual])
    r_strat3_failure <- append(r_strat3_failure, t_strat3_failure[individual])
    r_strat4_failure <- append(r_strat4_failure, t_strat4_failure[individual])
    r_teacher_ID <- append(r_teacher_ID, t_teacherID[individual])
    r_skill_attempted <- append(r_skill_attempted, t_skill_attempt[individual])
    r_SL_rate <- append(r_SL_rate, t_SL_rate[individual])
    r_SL_success <- append(r_SL_success, t_SL_success[individual])
    r_IL_rate <- append(r_IL_rate, t_IL_rate[individual])
    r_IL_success <- append(r_IL_success, t_IL_success[individual])
    r_n_resets <- append(r_n_resets, t_resets[individual])
    r_learner_payoffs <- append(r_learner_payoffs, t_learner_payoffs[individual])
    r_eligible_teachers <- append(r_eligible_teachers, t_eligible_teachers[individual])
    cat("timestep = ", t,"round = ", r, "\n")
  }
  
  n_skills <- append(n_skills, r_skills)
  ages <- append(ages, r_ages) 
  meta_strat <- append(meta_strat, r_meta_strat)
  learning_strat <- append(learning_strat, r_learning_strat)
  payoffs <- append(payoffs, r_payoffs) 
  learner_payoffs <- append(learner_payoffs, r_learner_payoffs)
  strat1_success <- append(strat1_success, r_strat1_success)
  strat1_failure <- append(strat1_failure, r_strat1_failure)
  strat2_success <- append(strat2_success, r_strat2_success)
  strat2_failure <- append(strat2_failure, r_strat2_failure)
  strat3_success <- append(strat3_success, r_strat3_success)
  strat3_failure <- append(strat3_failure, r_strat3_failure)
  strat4_success <- append(strat4_success, r_strat4_success)
  strat4_failure <- append(strat4_failure, r_strat4_failure)
  teacher_ID <- append(teacher_ID, r_teacher_ID)
  skill_attempted <- append(skill_attempted, r_skill_attempted)
  SL_rate <- append(SL_rate, r_SL_rate)
  SL_success <- append(SL_success, r_SL_success)
  n_resets <- append(n_resets, r_n_resets)
  IL_rate <- append(IL_rate, r_IL_rate)
  IL_success <- append(IL_success, r_IL_success)
  eligible_teachers <- append(eligible_teachers, r_eligible_teachers)
  ID <- append(ID, t_ID)
}


 overview <- data.frame(Round = n_rounds, 
                       Timestep = n_timestep, 
                       ID = ID, 
                       MetaStrat = meta_strat, 
                       LearningStrat = learning_strat, 
                       Skilllevel = n_skills,
                       Age = ages,
                       Payoff = learner_payoffs,
                       ILrate = IL_rate, 
                       ILsucess = IL_success,
                       SLrate = SL_rate,
                       SLsucess = SL_success,
                       PayoffLearningSuccess = strat1_success, 
                       PayoffLearningFails = strat1_failure, 
                       SimilarityLearningSuccess = strat2_success, 
                       SimilarityLearningFails = strat2_failure,
                       AgeLearningSuccess = strat3_success, 
                       AgeLearningFails = strat3_failure,
                       ConformityLearningSuccess = strat4_success, 
                       ConformityLearningFails = strat4_failure,
                       EligibleTeachers = eligible_teachers,
                       Teacher = teacher_ID,
                       AttemptedSkill = skill_attempted,
                       Resets = n_resets
)


num_columns <- ceiling(length(payoffs)/skills)
payoff_matrix <- matrix(payoffs, ncol = num_columns)
PayoffMatrix <- as.data.frame(payoff_matrix)
colnames(PayoffMatrix) <- paste0("Payoff_Round", 1:num_columns)

write.csv(overview, file = results_path, row.names = FALSE)
write.csv(PayoffMatrix, file = payoff_path, row.names = FALSE)
