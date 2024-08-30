# Source the R scripts ----

source("R/Node.R")
source("R/Graph.R")

# Simulate one node ----

sample_function <- function(x, y) {
  return(x + y)
}

node1 <- Node$new(name = "Node1", func = sample_function, args = list(1, 2))

node1$node_simulate(10, "output_path")

# Simulate simple graph ----
node_a <- Node$new(name = "A", func = function() rnorm(1, mean = 5, sd = 2))
node_b <- Node$new(name = "B", func = function(a) 1 + a, args = list(node_a))

# Create the graph with node_a and node_b
graph <- Graph$new(list(node_a, node_b))
output <- graph$simulate(num_samples = 100)
print(output)
