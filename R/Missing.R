Missing <- R6Class(
  "Missing",
  inherit = Node,  # Assuming _Node is analogous to Node in R
  public = list(
    underlying_value = NULL,
    index_node = NULL,

    initialize = function(name, underlying_value, index_node, visible = TRUE) {
      # Initialize the Missing node by calling the parent Node class's initialize method
      super$initialize(name = name, func = self$node_simulate, visible = visible)

      self$underlying_value <- underlying_value
      self$index_node <- index_node
      self$parents <- list(underlying_value, index_node)
      self$handle_multi_cols <- underlying_value$handle_multi_cols
      self$handle_multi_return <- underlying_value$handle_multi_return
    },

    node_simulate = function(...) {
      # Simulate missingness by replacing values in underlying_value$output based on index_node$output
      index_output <- self$index_node$output
      underlying_output <- self$underlying_value$output

      self$output <- mapply(function(x, y) if (!y) x else NA, underlying_output, index_output, SIMPLIFY = TRUE)
    }
  ),

  private = list(
    build_object = function(...) {
      # Create a Missing object with provided arguments
      do.call(self$new, list(...))
    }
  )
)
