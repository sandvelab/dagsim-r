# Source the R scripts ----

source("R/Node.R")
source("R/Graph.R")

# Simulate one node ----
sample_function <- function(x, y) {
  return(x + y)
}

node1 <- Node$new(name = "Node1", func = sample_function, args = list(1, 2))

node1$node_simulate(10)

# Simulate simple graph ----
node_a <- Node$new(name = "A", func = function() rnorm(1, mean = 5, sd = 2))
node_b <- Node$new(name = "B", func = function(a) 1 + a, args = list(node_a))
node_c <- Node$new(name = "C", func = function(a, b) b - a, args = list(node_b),
                   kwargs = list(a = node_a))

node_a$node_simulate(10)
node_b$node_simulate(10)
node_c$node_simulate(10)

graph <- Graph$new(list(node_a, node_b, node_c))
output <- graph$simulate(num_samples = 9)
graph$draw()
