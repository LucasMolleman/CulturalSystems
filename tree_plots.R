library(ggraph)
library(igraph)
library(grid)
source("simFunctions.R")
extrafont::loadfonts(device = "win")
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
  get_tr_sums = TRUE              # save trait distributions (Only do this if when running one simulation at a time)
)

tree <- generate_rooted_tree()
attr(tree, "requirements") <- trRequirements(tree)

### SYSTEM AND NODE PAYOFFS ARE SET
####### INITIALIZE POPULATION #####
blockers <- initializeBlockers(params, tree)
blockedTraits <- which(colSums(blockers) > 0)
blockedInds <- which(rowSums(blockers) > 0)
tree <- addDetours(params, tree, blockedTraits, type = params$detourType)
attr(tree, "requirements") <- augmentTrRequirements(tree) #add alternative routes around blocked traits
attr(tree, "blockedTraits") <- blockedTraits

requirements <- attributes(tree)$requirements
aux_nodes <- attributes(tree)$aux_nodes

adjust_layout <- function(tree) {
  #assumes trait 2 is blocked with numSteps = 3 and detourType = "parallel"
  layout <- layout.reingold.tilford(tree)
  layout[1,1] <- layout[2, 1]
  layout[17:19,1] <- layout[17:19,1] - 4
  layout[20:22,1] <- layout[20:22,1] - 3
  layout
}

edge_types <- function(tree) {
  blockedTraits <- attributes(tree)$blockedTraits
  aux_nodes <- attributes(tree)$aux_nodes
  edges_aux <- which(igraph::ends(tree, igraph::E(tree))[, 1] %in% aux_nodes | igraph::ends(tree, igraph::E(tree))[, 2] %in% aux_nodes)
  edges_blocked <- which(igraph::ends(tree, igraph::E(tree))[, 1] %in% blockedTraits | igraph::ends(tree, igraph::E(tree))[, 2] %in% blockedTraits)
  
  edge_types <- rep(0, length(E(tree)))
  edge_types[edges_aux] <- 1
  edge_types[edges_blocked] <- 2
  factor(edge_types)
}

node_types <- function(tree) {
  blockedTraits <- attributes(tree)$blockedTraits
  aux_nodes <- attributes(tree)$aux_nodes
  node_colors <- rep(0, vcount(tree))
  node_colors[aux_nodes] <- 1
  node_colors[blockedTraits] <- 2
  factor(node_colors, labels = c("Regular", "Auxiliary", "Blocked"))
}
E(tree)$type <- edge_types(tree)
V(tree)$type <- node_types(tree)

layer1 <- ggraph(tree, layout = adjust_layout(tree)) + 
  geom_edge_link(alpha = 1, aes(lty = as.factor(type), color = as.factor(type), filter = type %in% c(0, 2)), arrow = arrow(type = "closed", length = unit(2, "mm")), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm')) + 
  geom_edge_diagonal(alpha = 1, aes(lty = as.factor(type), color = as.factor(type), filter = E(tree)$type == 1), arrow = arrow(type = "closed", length = unit(2, "mm")), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm')) + 
  scale_edge_linetype_manual(values = c("solid", "dashed", "solid"), guide = "none") + 
  scale_edge_color_manual(values = c("black", "firebrick", "cornflowerblue"), guide = "none") +
  geom_node_point(size = 7, aes(color = type)) + 
  scale_color_manual(name = "Trait Type", values = c("black", "cornflowerblue", "firebrick")) + 
  theme(text = element_text(family = "Times New Roman"), panel.background = element_blank())


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
  blockedLayer = 2,               # layer at which tree blockages occur
  numBlocked = 1,                 # number of blocked traits in the model
  propBlocked = 0.1,              # proportion of individuals that have blocked traits
  numSteps = 3,                   # number of auxiliary traits necessary for overcoming a block
  detourType = "parallel",        # type of detour (parallel or serial)
  typical_learning_strategy = 1,  # social learning strategy used by unblocked learners
  get_tr_sums = TRUE              # save trait distributions (Only do this if when running one simulation at a time)
)

tree <- generate_rooted_tree()
attr(tree, "requirements") <- trRequirements(tree)

### SYSTEM AND NODE PAYOFFS ARE SET
####### INITIALIZE POPULATION #####
set.seed(1)
blockers <- initializeBlockers(params, tree)
blockedTraits <- which(colSums(blockers) > 0)
blockedInds <- which(rowSums(blockers) > 0)
tree <- addDetours(params, tree, blockedTraits, type = params$detourType)
attr(tree, "requirements") <- augmentTrRequirements(tree) #add alternative routes around blocked traits
attr(tree, "blockedTraits") <- blockedTraits

requirements <- attributes(tree)$requirements
aux_nodes <- attributes(tree)$aux_nodes

E(tree)$type <- edge_types(tree)
V(tree)$type <- node_types(tree)

adjust_layout_2 <- function(tree) {
  #assumes trait 3 is blocked with numSteps = 3 and detourType = "parallel"
  layout <- layout.reingold.tilford(tree)
  layout[3:16,1] <- layout[3:16, 1] + 4
  layout[17:19,1] <- layout[17:19,1] - 4
  layout[20:22,1] <- layout[20:22,1] - 3
  layout
}

layer2 <- ggraph(tree, layout = adjust_layout_2(tree)) + 
  geom_edge_link(alpha = 1, aes(lty = as.factor(type), color = as.factor(type), filter = type %in% c(0, 2)), arrow = arrow(type = "closed", length = unit(2, "mm")), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm')) + 
  geom_edge_diagonal(alpha = 1, aes(lty = as.factor(type), color = as.factor(type), filter = E(tree)$type == 1), arrow = arrow(type = "closed", length = unit(2, "mm")), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm')) + 
  scale_edge_linetype_manual(values = c("solid", "dashed", "solid"), guide = "none") + 
  scale_edge_color_manual(values = c("black", "firebrick", "cornflowerblue"), guide = "none") +
  geom_node_point(size = 7, aes(color = type)) + 
  scale_color_manual(name = "Trait Type", values = c("black", "cornflowerblue", "firebrick")) + 
  theme(text = element_text(family = "Times New Roman"), panel.background = element_blank())



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
  blockedLayer = 3,               # layer at which tree blockages occur
  numBlocked = 1,                 # number of blocked traits in the model
  propBlocked = 0.1,              # proportion of individuals that have blocked traits
  numSteps = 3,                   # number of auxiliary traits necessary for overcoming a block
  detourType = "parallel",        # type of detour (parallel or serial)
  typical_learning_strategy = 1,  # social learning strategy used by unblocked learners
  get_tr_sums = TRUE              # save trait distributions (Only do this if when running one simulation at a time)
)

tree <- generate_rooted_tree()
attr(tree, "requirements") <- trRequirements(tree)

### SYSTEM AND NODE PAYOFFS ARE SET
####### INITIALIZE POPULATION #####
set.seed(1)
blockers <- initializeBlockers(params, tree)
blockedTraits <- which(colSums(blockers) > 0)
blockedInds <- which(rowSums(blockers) > 0)
tree <- addDetours(params, tree, blockedTraits, type = params$detourType)
attr(tree, "requirements") <- augmentTrRequirements(tree) #add alternative routes around blocked traits
attr(tree, "blockedTraits") <- blockedTraits

requirements <- attributes(tree)$requirements
aux_nodes <- attributes(tree)$aux_nodes

E(tree)$type <- edge_types(tree)
V(tree)$type <- node_types(tree)

adjust_layout_3 <- function(tree) {
  #assumes trait 3 is blocked with numSteps = 3 and detourType = "parallel"
  layout <- layout.reingold.tilford(tree)
  layout[c(5,6,9:12),1] <- layout[c(5,6,9:12), 1] + 3.5
  #layout[c(3,4,7,8,13:16),1] <- layout[c(3,4,7,8,13:16), 1] + 1
  layout[17:19,1] <- layout[17:19,1] - 2.5
  layout[20:22,1] <- layout[20:22,1] + 0.25
  layout
}
adjust_layout_3(tree)
layer3 <- ggraph(tree, layout = adjust_layout_3(tree)) + 
  geom_edge_link(alpha = 1, aes(lty = as.factor(type), color = as.factor(type), filter = type %in% c(0, 2)), arrow = arrow(type = "closed", length = unit(2, "mm")), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm')) + 
  geom_edge_diagonal(alpha = 1, aes(lty = as.factor(type), color = as.factor(type), filter = E(tree)$type == 1), arrow = arrow(type = "closed", length = unit(2, "mm")), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm')) + 
  scale_edge_linetype_manual(values = c("solid", "dashed", "solid"), guide = "none") + 
  scale_edge_color_manual(values = c("black", "firebrick", "cornflowerblue"), guide = "none") +
  geom_node_point(size = 7, aes(color = type)) + 
  scale_color_manual(name = "Trait Type", values = c("black", "cornflowerblue", "firebrick")) + 
  theme(text = element_text(family = "Times New Roman"), panel.background = element_blank())

library(cowplot)
combined_plot <- plot_grid(
  layer1 + theme(legend.position = "none"),
  layer2 + theme(legend.position = "none"),
  layer3 + theme(legend.position = "none"),
  labels = c("A", "B", "C"), 
  label_size = 20,
  align = 'v', ncol = 1
)

legend <- get_legend(layer3)

final_plot <- plot_grid(combined_plot, legend, ncol = 2, rel_widths = c(1, 0.2))

print(final_plot)

