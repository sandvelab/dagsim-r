# Source the translated R scripts
source("R/Node.R")

# Define a sample function for nodes
sample_function <- function(x, y) {
  return(x + y)
}

# Create an instance of Node
node1 <- Node$new(name = "Node1", func = sample_function, args = list(1, 2))

# Simulate the node's behavior
node1$node_simulate(10, "output_path")
