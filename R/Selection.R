Selection <- R6Class(
  "Selection",
  inherit = Node,  # Assuming _Node is analogous to Node in R
  public = list(
    initialize = function(name, func, args = NULL, kwargs = NULL, visible = TRUE) {
      super$initialize(name = name, func = func, args = args, kwargs = kwargs, visible = visible)
    },

    filter_output = function(output_dict) {
      # Iterate through the output_dict and filter based on self$output
      for (key in names(output_dict)) {
        output_dict[[key]] <- output_dict[[key]][self$output]
      }
      return(output_dict)
    }
  ),

  private = list(
    build_object = function(...) {
      # Creating a Selection object with provided arguments
      do.call(self$new, list(...))
    }
  )
)
