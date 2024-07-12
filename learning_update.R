# Init --------------------------------------------------------------------

library(igraph)
parameters <- list(
  N = 100,
  num_nodes = 16,
  difficulty = list(mean = 1, sd = 0.5),
  intercept = list(mean = 2, sd = 1), 
  weights = list(mean = 1, sd = 0.5)
)

# Generate Tree -----------------------------------------------------------

generate_dag <- function(n) {
  g <- igraph::make_empty_graph(n, directed = TRUE) |>
    igraph::add_edges(combn(1:n, 2, simplify = TRUE)) |>
    igraph::add_edges(rep(1:n, each = 2), simplify = TRUE)
  return(g)
}

assign_random_weights <- function(graph, parameters) {
  igraph::E(graph)$weight <- rnorm(
    igraph::ecount(graph),
    mean = parameters$weights$mean,
    sd = parameters$weights$sd
  )
  igraph::E(graph)[igraph::ends(graph, igraph::E(graph))[, 1] == igraph::ends(graph, igraph::E(graph))[, 2]]$weight <- 1
  return(graph)
}

upstream_nodes <- function(node, graph) {
  setdiff(igraph::subcomponent(graph, node, mode = "in"), node)
}

tree <- generate_dag(parameters$num_nodes)
tree <- assign_random_weights(tree, parameters)
affordance_structure <- igraph::as_adjacency_matrix(tree, attr = "weight")
upstream_action_list <- purrr::map(seq_len(parameters$num_nodes), ~upstream_nodes(.x, tree))

# Individual differences --------------------------------------------------

generate_action_competence <- function(parameters) {
  matrix(runif(
    parameters$N * parameters$num_nodes,
    min = 0,
    max = 1
  ), nrow = parameters$N, ncol = parameters$num_nodes)
}

generate_action_difficulty <- function(parameters) {
  matrix(rnorm(
    parameters$N * parameters$num_nodes,
    mean = parameters$intercept$mean,
    sd = parameters$intercept$sd
  ), nrow = parameters$N, ncol = parameters$num_nodes)
}

generate_action_intercept <- function(parameters) {
  matrix(rnorm(
    parameters$N * parameters$num_nodes,
    mean = parameters$intercept$mean,
    sd = parameters$intercept$sd
  ), nrow = parameters$N, ncol = parameters$num_nodes)
}

action_competence <- generate_action_competence(parameters)
action_difficulty <- generate_action_difficulty(parameters)
action_intercept <- generate_action_intercept(parameters)

# Choose Actions ----------------------------------------------------------

choose_actions <- function() {
  #placeholder function
  sample(2:parameters$num_nodes, parameters$N, replace = TRUE)
}

chosen_actions <- choose_actions()
# Calculate summed competence ---------------------------------------------

upstream_actions <- 

get_competence_sums <- function(action_competence, affordance_structure, chosen_actions) {
  rowSums(action_competence * t(affordance_structure[ , chosen_actions])) 
}

competence_sums <- get_competence_sums(action_competence, affordance_structure, chosen_actions)


# Learning ----------------------------------------------------------------

learning_function <- function(competence_sum, difficulty = 1, intercept = -2, scale = 1) {
  scale * (1 / (1 + exp(-difficulty * (competence_sum - intercept))))
}

learning_outcomes <- learning_function(
  competence_sums,
  action_difficulty[cbind(seq_len(parameters$N), chosen_actions)],
  action_intercept[cbind(seq_len(parameters$N), chosen_actions)],
  1
)
hist(learning_outcomes, breaks = 20)
action_competence[cbind(seq_len(parameters$N), chosen_actions)] <- learning_outcomes



plot(NULL, xlim = c(0,5), ylim = c(0,1), xlab = "Experience", ylab = "Expertise", type = "n")

for (i in 1:100) {
  lines(0:5, learning_function(0:5, action_difficulty[i], action_intercept[i], 1), col = i)
}



range(competence_sums)

range(learning_outcomes)











