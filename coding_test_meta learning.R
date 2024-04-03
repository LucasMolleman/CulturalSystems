source("functions_and_libs_for_meta_strategies.R")

# set up population
population <- c(seq(1:500))
skills <- 20
timesteps <- 25000
rounds <- 10
reset_rate <- 0.05	
social_learning <- 0.99
selection_bonus <- 10
skill_probs <- c(rep(0.06, 12), rep(0.05, 3), rep(0.026, 5))

# sample teacher 
for (r in 1: rounds) {
  # give them first subskill
  skillset <- matrix(0, nrow = skills, ncol = length(population))
  for (i in 1:length(population)){
    number_skills <- sample(seq(1:20), 1, prob = skill_probs)
    # number_skills <- round(sample(runif(10000, 1, 20), 1), 0) 
  skillset[1:number_skills, i] <- 1
  } 
  skillset_start <- skillset
  
  payoffs <- c()
  for (s in 1:skills){
    payoffs_skill <- sample(rnorm(100, 1, 0.5), 1) * s
    # payoffs_skill<-runif(n = 1) * s 
    payoffs <- append(payoffs, payoffs_skill)
  }
  
  # set ages
  overview <- matrix(nrow = length(population), ncol = 10, dimnames = list(c(), c("Number_skills", "Age", "Learning_strat", "Successful", "Unsuccessful", "Number_skills_start", "Ind_Learning_success", "Ind_Learning_failure","resets", "Payoff")))
  overview[,"Age"] <- 0
  overview[,3:10] <- 0
  for (i in 1: ncol(skillset)){
    overview[i, "Number_skills"] <- sum(skillset[,i])
    overview[i, "Number_skills_start"] <- sum(skillset[,i])
    overview[i, "Age"] <- sum(skillset[,i])
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
  meta_learning()
  }
assign(paste0("skillset", r), skillset, envir = .GlobalEnv)
assign(paste0("overview", r), overview, envir = .GlobalEnv)
assign(paste0("meta_overview", r), meta_overview, envir = .GlobalEnv)
assign(paste0("over_time", r), over_time, envir = .GlobalEnv)
assign(paste0("skillset_start", r), skillset_start, envir = .GlobalEnv)
  }



## browser() to read it line by line

#### plotting etc. ------------------------------
colSums(overview1)
colSums(overview2)
colSums(overview3)
colSums(meta_overview3)

overview_dat1 <- as.data.frame(overview1)
overview_dat2 <- as.data.frame(overview2)
overview_dat3 <- as.data.frame(overview3)
overview_dat4 <- as.data.frame(overview4)
overview_dat5 <- as.data.frame(overview5)
overview_dat6 <- as.data.frame(overview6)
overview_dat7 <- as.data.frame(overview7)
overview_dat8 <- as.data.frame(overview8)
overview_dat9 <- as.data.frame(overview9)
overview_dat10 <- as.data.frame(overview10)

overall_overview <- rbind.data.frame(
  overview_dat1,
  overview_dat2,
  overview_dat3,
  overview_dat4,
  overview_dat5,
  overview_dat6,
  overview_dat7,
  overview_dat8,
  overview_dat9,
  overview_dat10
)

mean(overall_overview$Number_skills)
mean(overall_overview$Number_skills_start)
mean(overall_overview$resets)
mean(overall_overview$Ind_Learning_success)
mean(overall_overview$Ind_Learning_failure)
mean(overall_overview$Payoff)

sum(overall_overview$Ind_Learning_success, overall_overview$Ind_Learning_failure)
sum(overall_overview$Ind_Learning_failure)/sum(overall_overview$Ind_Learning_success, overall_overview$Ind_Learning_failure)

overall_meta_overview <- rbind.data.frame(
  meta_overview1,
  meta_overview2,
  meta_overview3,
  meta_overview4,
  meta_overview5, 
  meta_overview6,
  meta_overview7,
  meta_overview8,
  meta_overview9,
  meta_overview10
)

overall_meta_overview %>%
  filter(Meta_strategy == 1) %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(3:4), list(Mean = mean))
overall_meta_overview %>%
  group_by(Learning_strat) %>%
  summarise_at(vars(3:4), list(Mean = mean))
overall_meta_overview %>%
  group_by(Meta_strategy) %>%
  summarise_at(vars(3:4), list(Mean = mean))

#####

# ID_list_meta1 <- get_unique_ID(list(meta_overview1, meta_overview2, meta_overview3, meta_overview4, meta_overview5,
#                               meta_overview6, meta_overview7, meta_overview8, meta_overview9, meta_overview10),
#                          1, 10)
# result_meta1 <- calculate_mean_and_var(list(overview_dat1, overview_dat2, overview_dat3, overview_dat4, overview_dat5,
#                                       overview_dat6, overview_dat7, overview_dat8, overview_dat9, overview_dat10),
#                                  ID_list_meta1)
# mean_meta1 <- result_meta1$mean_meta
# var_meta1 <- result_meta1$var_meta
# values_meta1 <- result_meta1$values_meta
# 
# ID_list_meta2 <- get_unique_ID(list(meta_overview1, meta_overview2, meta_overview3, meta_overview4, meta_overview5,
#                                     meta_overview6, meta_overview7, meta_overview8, meta_overview9, meta_overview10),
#                                2, 10)
# result_meta2 <- calculate_mean_and_var(list(overview_dat1, overview_dat2, overview_dat3, overview_dat4, overview_dat5,
#                                             overview_dat6, overview_dat7, overview_dat8, overview_dat9, overview_dat10),
#                                        ID_list_meta2)
# mean_meta2 <- result_meta2$mean_meta
# var_meta2 <- result_meta2$var_meta
# values_meta2 <- result_meta2$values_meta

####

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

#### make a plot for each of the learning strat


  ### for payoff
  payoff_meta1_run1 <- overview_dat1[ID_strat1_run1, "Payoff"]
  payoff_meta1_run2 <- overview_dat2[ID_strat1_run2, "Payoff"]
  payoff_meta1_run3 <- overview_dat3[ID_strat1_run3, "Payoff"]
  payoff_meta1_run4 <- overview_dat4[ID_strat1_run4, "Payoff"]
  payoff_meta1_run5 <- overview_dat5[ID_strat1_run5, "Payoff"]
  payoff_meta1_run6 <- overview_dat6[ID_strat1_run6, "Payoff"]
  payoff_meta1_run7 <- overview_dat7[ID_strat1_run7, "Payoff"]
  payoff_meta1_run8 <- overview_dat8[ID_strat1_run8, "Payoff"]
  payoff_meta1_run9 <- overview_dat9[ID_strat1_run9, "Payoff"]
  payoff_meta1_run10 <- overview_dat10[ID_strat1_run10, "Payoff"]

  payoffs_meta1 <- c(
    payoff_meta1_run1,
    payoff_meta1_run2,
    payoff_meta1_run3,
    payoff_meta1_run4,
    payoff_meta1_run5,
    payoff_meta1_run6,
    payoff_meta1_run7,
    payoff_meta1_run8,
    payoff_meta1_run9,
    payoff_meta1_run10
  )

  mean_payoffs_meta1 <- mean(payoffs_meta1)
  var_payoffs_meta1 <- var(payoffs_meta1)

  payoff_meta2_run1 <- overview_dat1[ID_strat2_run1, "Payoff"]
  payoff_meta2_run2 <- overview_dat2[ID_strat2_run2, "Payoff"]
  payoff_meta2_run3 <- overview_dat3[ID_strat2_run3, "Payoff"]
  payoff_meta2_run4 <- overview_dat4[ID_strat2_run4, "Payoff"]
  payoff_meta2_run5 <- overview_dat5[ID_strat2_run5, "Payoff"]
  payoff_meta2_run6 <- overview_dat6[ID_strat2_run6, "Payoff"]
  payoff_meta2_run7 <- overview_dat7[ID_strat2_run7, "Payoff"]
  payoff_meta2_run8 <- overview_dat8[ID_strat2_run8, "Payoff"]
  payoff_meta2_run9 <- overview_dat9[ID_strat2_run9, "Payoff"]
  payoff_meta2_run10 <- overview_dat10[ID_strat2_run10, "Payoff"]

  payoffs_meta2 <- c(
    payoff_meta2_run1,
    payoff_meta2_run2,
    payoff_meta2_run3,
    payoff_meta2_run4,
    payoff_meta2_run5,
    payoff_meta2_run6,
    payoff_meta2_run7,
    payoff_meta2_run8,
    payoff_meta2_run9,
    payoff_meta2_run10
  )

  mean_payoffs_meta2 <- mean(payoffs_meta2)
  var_payoffs_meta2 <- var(payoffs_meta2)

  payoff_meta3_run1 <- overview_dat1[ID_strat3_run1, "Payoff"]
  payoff_meta3_run2 <- overview_dat2[ID_strat3_run2, "Payoff"]
  payoff_meta3_run3 <- overview_dat3[ID_strat3_run2, "Payoff"]
  payoff_meta3_run4 <- overview_dat4[ID_strat3_run4, "Payoff"]
  payoff_meta3_run5 <- overview_dat5[ID_strat3_run5, "Payoff"]
  payoff_meta3_run6 <- overview_dat6[ID_strat3_run6, "Payoff"]
  payoff_meta3_run7 <- overview_dat7[ID_strat3_run7, "Payoff"]
  payoff_meta3_run8 <- overview_dat8[ID_strat3_run8, "Payoff"]
  payoff_meta3_run9 <- overview_dat9[ID_strat3_run9, "Payoff"]
  payoff_meta3_run10 <- overview_dat10[ID_strat3_run10, "Payoff"]

  payoffs_meta3 <- c(
    payoff_meta3_run1,
    payoff_meta3_run2,
    payoff_meta3_run3,
    payoff_meta3_run4,
    payoff_meta3_run5,
    payoff_meta3_run6,
    payoff_meta3_run7,
    payoff_meta3_run8,
    payoff_meta3_run9,
    payoff_meta3_run10
  )

  mean_payoffs_meta3 <- mean(payoffs_meta3)
  var_payoffs_meta3 <- var(payoffs_meta3)

  payoff_means <- c(mean_payoffs_meta1, mean_payoffs_meta2, mean_payoffs_meta3)
  payoff_vars <- c(var_payoffs_meta1, var_payoffs_meta2, var_payoffs_meta3)

  boxplot(payoffs_meta1, payoffs_meta2, payoffs_meta3, names = c("Strat for life", "Bayesian Learner", "MoE"),
          main = "Payoffs",
          ylab = "Payoff")

  # Add mean points to the plot
  points(1:3, payoff_means, col = "red", pch = 19)

  # Add error bars representing variance
  arrows(1:3, payoff_means - sqrt(payoff_vars), 1:3, payoff_means + sqrt(payoff_vars),
         angle = 90, code = 3, length = 0.1, col = "blue")




### plot bayesian learner development over time
# plot the usage of learning strategies over time
time_overview1 <- as.data.frame(matrix(over_time1, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview2 <- as.data.frame(matrix(over_time2, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview3 <- as.data.frame(matrix(over_time3, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview4 <- as.data.frame(matrix(over_time4, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview5 <- as.data.frame(matrix(over_time5, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview6 <- as.data.frame(matrix(over_time6, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview7 <- as.data.frame(matrix(over_time7, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview8 <- as.data.frame(matrix(over_time8, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview9 <- as.data.frame(matrix(over_time9, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
time_overview10 <- as.data.frame(matrix(over_time10, ncol=3, byrow = TRUE, dimnames = list(c(), c("ID", "Age", "Strategy"))))
  
time_overview_sorted1 <- time_overview1 %>%
  arrange(ID)
time_overview_sorted2 <- time_overview2 %>%
  arrange(ID)
time_overview_sorted3 <- time_overview3 %>%
  arrange(ID)
time_overview_sorted4 <- time_overview4 %>%
  arrange(ID)
time_overview_sorted5 <- time_overview5 %>%
  arrange(ID)
time_overview_sorted6 <- time_overview6 %>%
  arrange(ID)
time_overview_sorted7 <- time_overview7 %>%
  arrange(ID)
time_overview_sorted8 <- time_overview8 %>%
  arrange(ID)
time_overview_sorted9 <- time_overview9 %>%
  arrange(ID)
time_overview_sorted10 <- time_overview10 %>%
  arrange(ID)

time_overv_sorted_all <- rbind.data.frame(
  time_overview_sorted1, 
  time_overview_sorted2, 
  time_overview_sorted3, 
  time_overview_sorted4, 
  time_overview_sorted5, 
  time_overview_sorted6, 
  time_overview_sorted7, 
  time_overview_sorted8, 
  time_overview_sorted9, 
  time_overview_sorted10
)


time_overview_examples <- time_overview_sorted1[1:4000,]

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

df_counts <- time_overv_sorted_all %>%
  group_by(Age) %>%
  summarize(Count_all = n())

df_time <- left_join(df_counts, time_overv_sorted_all) %>%
  group_by(Age, Strategy) %>%
  summarize(percent = n()/mean(Count_all))

# Plot count of each value for each round
ggplot(df_time, aes(x = Age, y = percent, color = as.factor(Strategy))) +
  geom_point() + 
  geom_line() + 
  theme_classic() +
  scale_color_discrete(name="Learning strategy")

###
?geom_line



plot(density(overall_overview$Number_skills_start))
plot(density(overall_overview$Number_skills))

### 
skill_overview_meta1 <- rbind(
  overview_dat1[ID_strat1_run1, ],
  overview_dat2[ID_strat1_run2, ],
  overview_dat3[ID_strat1_run3, ],
  overview_dat4[ID_strat1_run4, ],
  overview_dat5[ID_strat1_run5, ],
  overview_dat6[ID_strat1_run6, ],
  overview_dat7[ID_strat1_run7, ],
  overview_dat8[ID_strat1_run8, ],
  overview_dat9[ID_strat1_run9, ],
  overview_dat10[ID_strat1_run10, ]
)

skill_overview_meta1_means <- skill_overview_meta1 %>%
  select(Number_skills, Number_skills_start) %>%
  mutate(difference = skill_overview_meta1$Number_skills - skill_overview_meta1$Number_skills_start) %>%
  colMeans()

skill_overview_meta2 <- rbind(
  overview_dat1[ID_strat2_run1, ],
  overview_dat2[ID_strat2_run2, ],
  overview_dat3[ID_strat2_run3, ],
  overview_dat4[ID_strat2_run4, ],
  overview_dat5[ID_strat2_run5, ],
  overview_dat6[ID_strat2_run6, ],
  overview_dat7[ID_strat2_run7, ],
  overview_dat8[ID_strat2_run8, ],
  overview_dat9[ID_strat2_run9, ],
  overview_dat10[ID_strat2_run10, ]
)

skill_overview_meta2_means <- skill_overview_meta2 %>%
  select(Number_skills, Number_skills_start) %>%
  mutate(difference = skill_overview_meta2$Number_skills - skill_overview_meta2$Number_skills_start) %>%
  colMeans()

skill_overview_meta3 <- rbind(
  overview_dat1[ID_strat3_run1, ],
  overview_dat2[ID_strat3_run2, ],
  overview_dat3[ID_strat3_run3, ],
  overview_dat4[ID_strat3_run4, ],
  overview_dat5[ID_strat3_run5, ],
  overview_dat6[ID_strat3_run6, ],
  overview_dat7[ID_strat3_run7, ],
  overview_dat8[ID_strat3_run8, ],
  overview_dat9[ID_strat3_run9, ],
  overview_dat10[ID_strat3_run10, ]
)

skill_overview_meta3_means <- skill_overview_meta3 %>%
  select(Number_skills, Number_skills_start) %>%
  mutate(difference = skill_overview_meta3$Number_skills - skill_overview_meta3$Number_skills_start) %>%
  colMeans()

skill_overview_meta1_means
skill_overview_meta2_means
skill_overview_meta3_means


plot(density(skill_overview_meta1$Number_skills_start), col = "green")
lines(density(skill_overview_meta2$Number_skills_start), col = "blue")
lines(density(skill_overview_meta3$Number_skills_start), col = "red")

plot(density(skill_overview_meta1$Number_skills), col = "green")
lines(density(skill_overview_meta2$Number_skills), col = "blue")
lines(density(skill_overview_meta3$Number_skills), col = "red")


# include fixed death rate 
# play around with payoffs
# animate pop skill level over time
# look at the age-based learning weight

