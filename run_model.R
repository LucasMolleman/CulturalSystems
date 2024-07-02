library(igraph)
library(dplyr)


source("simFunctions.R")
source("loopFunctions.R")

#### simulation parameters
params <- list(
  num_nodes = 16,                 # size of the cultural systems (number of nodes)
  root_node = 1,                  # root node of the tree
  N = 100,                        # population size
  M = 10,                         # number of demonstrators
  timesteps =  2000,              # number of time steps in the simulation
  S = 0.99,                       # reliance on social learning; (1-S) is innovation rate
  reset_rate = 0.05,              # probability that an individual is replaced by a naive individual
  payoff_scaling = 2,             # Constant that is added/subtracted for each step away from the root node
  payoff_weight = 1,              # How much payoff depends on randomness vs distance from the root node (0=random, 1=deterministic))
  blockedLayer = 1,               # layer at which tree blockages occur
  numBlocked = 1,                 # number of blocked traits in the model
  propBlocked = 0.1,              # proportion of individuals that have blocked traits
  numSteps = 3,                   # number of auxiliary traits necessary for overcoming a block
  detourType = "parallel",        # type of detour (parallel or serial)
  typical_learning_strategy = 1,  # social learning strategy used by unblocked learners
  get_tr_sums = TRUE,             # save trait distributions (Only do this if when running one simulation at a time)
  data_path = "./data/"
)

## Parallel Execution ----------------------------------------------------------------------------------------------------

set.seed(1)
tree <- generate_rooted_tree()
attr(tree, "requirements") <- trRequirements(tree)
future::plan(future::multisession, workers = 128)

start_time <- Sys.time()
expand.grid(learningStrategy = c(0:2),
            numSteps = c(1, 2, 3),
            blockedLayer = c(1, 2, 3),
            propBlocked = round(seq(0.01, 1, length.out = 50),2),
            repl = 1:3) %>%
run_all_simulations_parallel(params, tree) %>%
saveRDS("strategySuccess.rds")
end_time <- Sys.time()
future::plan(future::sequential)
end_time - start_time

## Sequential Execution ---------------------------------------------------------------------------------------------------
# set.seed(1)
# tree <- generate_rooted_tree()
# attr(tree, "requirements") <- trRequirements(tree)
# 
# expand.grid(learningStrategy = c(1),
#             numSteps = c(2),
#             blockedLayer = c(1),
#             propBlocked = c(0.1),
#             repl = 1:30) %>%
# run_all_simulations(params, tree) %>%
# saveRDS("strategySuccess.rds")
# 
# ## Create Figure -----------------------------------------------------------------------------------------------------------
# 
# strategySuccess <- readRDS("./data/strategySuccess.rds")
# strategySuccess <- read.csv("../CulturalSystemscpp/SimulationResults.csv", na.strings = "nan")
# data <- as.data.frame(strategySuccess)
# library(data.table)
# setDT(data)
# setnames(data, c("numNodes", "blockedLearningStrat", "repl", "payoff_scaling", "blockedLayer", "numBlocked", "numSteps",'propBlocked', "sumPayoff", "sumPayoffUnblocked", "sumPayoffBlocked", "failed_learning_prop", "failed_learning_prop_blocked"))
# 
# #set first 8 columns to factor
# data[ , (1:5) := lapply(.SD, factor), .SDcols = 1:5]
# 
# data[ , blockedLearningStrat := factor(blockedLearningStrat, levels = c(0,1,2,3,4,7), labels = c("Random", "Payoff", "Similarity", "Age-Based", "Conformity", "Similarity2"))] # nolint
# 
# summary(mod1 <- lm(sumPayoffBlocked ~ blockedLearningStrat + numSteps + blockedLayer + propBlocked , data = data))
# 
# summary(mod2 <- lm(sumPayoffBlocked ~ blockedLearningStrat*blockedLayer*propBlocked, data = data))
# 
# #Probing the interaction numSteps:blockedLayer:propBlocked 
# 
# prepare_aggregated_data <- function(data, dependent_var, independent_vars) {
#   aggregated_data <- data %>%
#     dplyr::group_by(across(all_of(independent_vars))) %>%
#     dplyr::summarise(
#         mean_value = mean(get(dependent_var), na.rm = TRUE),
#         SE = sd(get(dependent_var), na.rm = TRUE) / sqrt(n()),  # Standard Error
#         n = n(),
#         .groups = 'drop'
#       ) %>%
#     dplyr::mutate(
#       CI_lower = mean_value - (1.96 * SE),
#       CI_upper = mean_value + (1.96 * SE)
#     )
#   
#   return(aggregated_data)
# }
# 
# dependent_var <- "sumPayoffBlocked"
# independent_vars <- c("blockedLearningStrat", "numSteps", "blockedLayer", "propBlocked")
# 
# aggregated_data2 <- prepare_aggregated_data(data, dependent_var, independent_vars)
# 
# # Plotting the aggregated means
# ggplot2::ggplot(aggregated_data2, aes(x = as.factor(numSteps), y = mean_value, group = interaction(blockedLayer, propBlocked), color = as.factor(blockedLayer))) +
#   ggplot2::geom_line(aes(linetype = as.factor(propBlocked))) +
#   ggplot2::geom_point(aes(shape = as.factor(propBlocked))) +
#   ggplot2::geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +  # 95% CI error bars
#   ggplot2::facet_wrap(~blockedLearningStrat, labeller = labeller(blockedLearningStrat = c(Random = "Strategy: Random", Payoff = "Strategy: Payoff", Similarity = "Strategy: Similarity", Similarity2 = "Strategy: Similiarity 2"))) +
#   ggplot2::labs(title = "Average SumPayoffBlocked across NumSteps, BlockedLayer, PropBlocked,\n and LearningStrategies",
#                 subtitle = "Data aggregated across unique combinations of factors",
#                 x = "Number of Steps",
#                 y = "Average Sum Payoff Blocked",
#                 color = "Blocked Layer",
#                 linetype = "Proportion Blocked",
#                 shape = "Proportion Blocked") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate X labels for better visualization
#         strip.text.x = element_text(size = 12, face = "bold"))