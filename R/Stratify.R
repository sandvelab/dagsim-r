Stratify <- R6Class(
  "Stratify",
  inherit = Node,  # Assuming _Node is analogous to Node in R
  public = list(
    initialize = function(name, func, args = NULL, kwargs = NULL, visible = TRUE) {
      super$initialize(name = name, func = func, args = args, kwargs = kwargs, visible = visible)
    },

    filter_output = function(output_dict) {
      # Get the names of the nodes
      node_names <- names(output_dict)

      # Determine the strata
      strata <- unique(self$output)

      # Initialize a nested list: outer list keyed by strata, inner lists keyed by node names
      new_dict <- lapply(strata, function(stratum) {
        setNames(vector("list", length(node_names)), node_names)
      })
      names(new_dict) <- strata

      # Populate the new_dict with stratified output
      for (i in seq_along(self$output)) {
        stratum <- self$output[[i]]
        for (k in node_names) {
          new_dict[[stratum]][[k]] <- c(new_dict[[stratum]][[k]], output_dict[[k]][[i]])
        }
      }

      return(new_dict)
    }
  ),

  private = list(
    build_object = function(...) {
      # Creating a Stratify object with provided arguments
      do.call(self$new, list(...))
    }
  )
)
