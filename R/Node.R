library(R6)

NodeBase <- R6Class(
  "NodeBase",
  public = list(
    name = NULL,
    func = NULL,
    func_name = NULL,
    plates = list(),
    observed = TRUE,
    args = list(),
    kwargs = list(),
    arg_getters = list(),
    kwarg_getters = list(),
    visible = TRUE,
    parents = list(),

    output = NULL,

    initialize = function(name, func, func_name, plates = list(), observed = TRUE, args = list(), kwargs = list(), visible = TRUE) {
      # Ensure empty list if NULL provided
      if (is.null(plates)) plates <- list()
      if (is.null(args)) args <- list()
      if (is.null(kwargs)) kwargs <- list()

      self$name <- name
      self$func <- func
      self$func_name <- func_name
      self$plates <- plates
      self$args <- args
      self$kwargs <- kwargs
      self$observed <- observed

      # Convert args and kwargs into functions that can dynamically fetch the outputs of other nodes
      self$arg_getters <- lapply(args, function(arg) {
        if (inherits(arg, "NodeBase")) {
          function(index) arg$output[[index]]
        } else {
          function(index) arg
        }
      })

      self$kwarg_getters <- lapply(kwargs, function(kwarg) {
        if (inherits(kwarg, "NodeBase")) {
          function(index) kwarg$output[[index]]
        } else {
          function(index) kwarg
        }
      })

      self$update_parents()
    },

    update_parents = function() {
      self$parents <- unique(c(
        lapply(self$args, function(a) if (inherits(a, "NodeBase")) a),
        lapply(self$kwargs, function(v) if (inherits(v, "NodeBase")) v)
      ))
      self$parents <- Filter(Negate(is.null), self$parents)
    },

    get_args = function(index) {
      return(lapply(self$arg_getters, function(a) a(index)))
    },

    get_kwargs = function(index) {
      return(lapply(self$kwarg_getters, function(v) v(index)))
    },

    forward = function(idx) {
      do.call(self$func, c(self$get_args(idx), self$get_kwargs(idx)))
    },

    node_simulate = function(num_samples) {
      self$output <- lapply(1:num_samples, function(i) self$forward(i))
    },

    print = function() {
      cat("Node:\n")
      cat("  Name: ", self$name, "\n")
      cat("  Type: ", class(self)[1], "\n")
      cat("  Function: ", self$func_name, "\n")
      if (length(self$parents) > 0) {
        cat("  Parents: ", paste(sapply(self$parents, function(p) p$name), collapse = ", "), "\n")
      } else {
        cat("  Parents: None\n")
      }
    }
  )
)

Node <- R6Class(
  "Node",
  inherit = NodeBase,
  public = list(
    handle_multi_cols = FALSE,
    handle_multi_return = NULL,

    initialize = function(name, func, args = NULL, kwargs = NULL, plates = NULL, size_field = NULL,
                          observed = TRUE, visible = TRUE, handle_multi_cols = FALSE, handle_multi_return = NULL) {
      super$initialize(name, func, func_name = deparse(substitute(func)),  plates = plates, observed = observed, args = args, kwargs = kwargs, visible = visible)
      self$handle_multi_cols <- handle_multi_cols
      self$handle_multi_return <- handle_multi_return
    },

    build_object = function(...) {
      Node$new(...)
    }
  )
)
