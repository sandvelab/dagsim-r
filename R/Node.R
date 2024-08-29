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
    size_field = NULL,
    visible = TRUE,
    parents = list(),
    output = NULL,

    initialize = function(name, func, func_name, plates = NULL, observed = TRUE, args = NULL, kwargs = NULL,
                          size_field = NULL, visible = TRUE) {
      if (is.null(args)) args <- list()
      if (is.null(kwargs)) kwargs <- list()
      if (is.null(plates)) plates <- list()

      self$name <- name
      self$func <- func
      self$func_name <- deparse(substitute(func))
      self$plates <- plates
      self$observed <- observed
      self$visible <- visible
      self$size_field <- size_field

      # Convert args and kwargs into functions that can dynamically fetch the outputs of other nodes
      self$args <- lapply(args, function(a) {
        if (inherits(a, "NodeBase")) {
          function(index) a$output[[index]]
        } else {
          function(index) a
        }
      })

      self$kwargs <- lapply(kwargs, function(v) {
        if (inherits(v, "NodeBase")) {
          function(index) v$output[[index]]
        } else {
          function(index) v
        }
      })

      self$update_parents()
    },

    update_parents = function() {
      self$parents <- unique(c(
        lapply(self$args, function(a) if (inherits(a, "NodeBase")) a),
        lapply(self$kwargs, function(v) if (inherits(v, "NodeBase")) v)
      ))
    },

    get_func_args = function(index) {
      lapply(self$args, function(a) a(index))
    },

    get_func_kwrgs = function(index, output_path) {
      d <- lapply(self$kwargs, function(v) v(index))
      if ("output_path" %in% names(formals(self$func))) {
        d$output_path <- output_path
      }
      return(d)
    },

    forward = function(idx, output_path) {
      do.call(self$func, c(self$get_func_args(idx), self$get_func_kwrgs(idx, output_path)))
    },

    node_simulate = function(num_samples, output_path) {
      if (is.null(self$size_field)) {
        self$output <- lapply(1:num_samples, function(i) self$forward(i, output_path))
      } else {
        self$output <- self$vectorize_forward(num_samples, output_path)
      }
    },

    vectorize_forward = function(size, output_path) {
      do.call(self$func, c(self$get_func_args(1), self$get_func_kwrgs(1, output_path), list(size_field = size)))
    },

    print = function() {
      cat("Node:\n")
      cat("  Name: ", self$name, "\n")
      cat("  Type: ", class(self)[1], "\n")
      cat("  Function: ", deparse(substitute(self$func)), "\n")
      if (length(self$parents) > 0) {
        cat("  Parents: ", paste(sapply(self$parents, function(p) p$name), collapse = ", "), "\n")
      } else {
        cat("  Parents: None\n")
      }
    },

    length = function() {
      length(self$parents)
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
      super$initialize(name, func, func_name = deparse(substitute(func)),  plates = plates, observed = observed, args = args, kwargs = kwargs,
                       size_field = size_field, visible = visible)
      self$handle_multi_cols <- handle_multi_cols
      self$handle_multi_return <- handle_multi_return
    },

    build_object = function(...) {
      Node$new(...)
    }
  )
)
