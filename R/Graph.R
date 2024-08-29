library(R6)

Graph <- R6Class(
  "Graph",
  public = list(
    nodes = list(),

    add_node = function(node) {
      self$nodes <- c(self$nodes, list(node))
    },

    get_nodes = function() {
      return(self$nodes)
    }
  )
)
