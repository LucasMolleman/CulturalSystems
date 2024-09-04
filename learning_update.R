# Init --------------------------------------------------------------------

library(igraph)
parameters <- list(
  N = 100,
  num_nodes = 16,
  difficulty = list(mean = 1, sd = 0.5),
  intercept = list(mean = 2, sd = 1), 
  weights = list(mean = 0.5, sd = 0.5),
  clustering_ratio = 0.1,
  clustering_strength = 10
)

# Generate Tree -----------------------------------------------------------

generate_dag <- function(n) {
  edges <- unlist(lapply(1:(n-1), function(i) {
    lapply((i+1):n, function(j) {
      c(i, j)
    })
  }), recursive = FALSE)
  edges <- do.call(rbind, edges)
  g <- igraph::make_empty_graph(n, directed = TRUE) |>
    igraph::add_edges(t(edges))
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

assign_random_weights_pruned <- function(graph, parameters) {
  igraph::E(graph)$weight <- rnorm(
    igraph::ecount(graph),
    mean = parameters$weights$mean,
    sd = parameters$weights$sd
  )
  
  igraph::E(graph)$weight[igraph::E(graph)$weight < 0.5] <- 0
  graph <- igraph::delete_edges(graph, igraph::E(graph)[igraph::E(graph)$weight == 0])
  igraph::E(graph)[igraph::ends(graph, igraph::E(graph))[, 1] == igraph::ends(graph, igraph::E(graph))[, 2]]$weight <- 1
  return(graph)
}

upstream_nodes <- function(node, graph) {
  setdiff(igraph::subcomponent(graph, node, mode = "in"), node)
}

get_labels <- function(graph) {
  in_degree <- igraph::degree(graph, mode = "in")
  out_degree <- igraph::degree(graph, mode = "out")
  labels <- paste(in_degree, out_degree, sep = "|")
  return(labels)
}
color_edges_by_strength <- function(graph) {
  strengths <- igraph::E(graph)$weight
  colors <- sapply(strengths, function(w) {
    if (w < 0.25) {
      return("lightblue")
    } else if (w <= 0.75) {
      return("lightgreen")
    } else {
      return("salmon")
    }
  })
  igraph::E(graph)$color <- colors
  return(graph)
}



  
tree <- generate_dag(parameters$num_nodes)
igraph::is_dag(tree)


dag_layout <- layout.fruchterman.reingold(tree)
plot(tree, layout = dag_layout, edge.arrow.size = 0.5, vertex.label = get_labels(tree), vertex.label.cex = 0.7)


tree <- assign_random_weights(tree, parameters)
tree <- color_edges_by_strength(tree)
plot(tree, layout = dag_layout, edge.arrow.size = 0.5, vertex.label = get_labels(tree), vertex.label.cex = 0.7)
#legend("topright", legend = c("Weak", "Medium", "Strong"), col = c("lightblue", "lightgreen", "salmon"), lty = 1, lwd = 3, title = "Connection Strength")


tree <- assign_random_weights_pruned(tree, parameters)
tree <- color_edges_by_strength(tree)
igraph::is_dag(tree)
plot(tree, layout = dag_layout, edge.arrow.size = 0.5, vertex.label = get_labels(tree), vertex.label.cex = 0.7)
affordance_structure <- igraph::as_adjacency_matrix(tree, attr = "weight")
upstream_action_list <- purrr::map(seq_len(parameters$num_nodes), ~upstream_nodes(.x, tree))
round(as_adjacency_matrix(tree, attr = "weight"),1)
colSums(as.matrix(as_adjacency_matrix(tree, attr = "weight")))
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

x_range <- seq(-10, 10, length.out=100)
par(mar = c(3, 3, 3, ))
# Plot setup
plot(x_range, 1 / (1 + exp(-0.3 * x_range)), type="l", col="blue", 
     xlab="Competence in Prerequisite Actions", 
     ylab="Learning Outcome", 
     xaxt='n', yaxt='n', bty = "n")



sigmoid <- function(x)  1 / (1 + exp(-0.3 * x))
sigmoid_values <- sigmoid(x_range)

brick_wall_height <- sigmoid_values[20]
sigmoid_res <- sigmoid_values[21:80]
brick_wall_values <- c(sigmoid_values[1:20], rep(brick_wall_height, 20), sigmoid_values[21:80])
plot(x_range, brick_wall_values, type="l", col="blue", 
     xlab="Competence in Prerequisite Actions", 
     ylab="Learning Outcome", 
     xaxt='n', yaxt='n')



plot(NULL, xlim = c(0,5), ylim = c(0,1), xlab = "Experience", ylab = "Expertise", type = "n")

for (i in 1:100) {
  lines(0:5, learning_function(0:5, action_difficulty[i], action_intercept[i], 1), col = i)
}



# Idea for feedback: Sum of weighed means over time with an increasingly wide
# window up the number of past timesteps, the weighing is inversely related to
# the number of timesteps over which the sample is taken







