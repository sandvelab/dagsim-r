Graph <- R6Class(
  "Graph",
  public = list(
    name = NULL,
    nodes = NULL,
    plates = NULL,
    adj_mat = NULL,
    top_order = NULL,
    folded_dot_str = NULL,
    unfolded_dot_str = NULL,

    initialize = function(list_nodes, name = "Graph", plates_reps = NULL) {
      self$check_args(list_nodes)
      self$name <- name
      self$nodes <- list_nodes
      self$plates <- list()
      self$adj_mat <- data.frame()
      self$top_order <- character()
      self$update_topol_order()
      self$update_plate_embedding()
      self$folded_dot_str <- self$generate_dot()
      if (!is.null(plates_reps)) {
        self$unfolded_dot_str <- ""
        self$unfold_graph(plates_reps)
      }
    },

    unfold_graph = function(reps) {
      removed_nodes <- self$replicate_nodes(reps)
      self$update_topol_order()
      self$update_nodes(removed_nodes)
      self$update_topol_order()
      self$update_plate_embedding()
      self$unfolded_dot_str <- self$generate_dot()
    },

    check_args = function(list_nodes) {
      if (sum(sapply(list_nodes, function(node) inherits(node, "Selection"))) > 1) {
        stop("A graph can have at most one Selection node.")
      }
      if (sum(sapply(list_nodes, function(node) inherits(node, "Stratify"))) > 1) {
        stop("A graph can have at most one Stratify node.")
      }
    },

    get_nodes_to_aggregate = function() {
      nodes_to_aggregate <- list()
      for (node in self$nodes) {
        for (parent in node$parents) {
          node_plates <- unique(node$plates)
          parent_plates <- unique(parent$plates)
          if (length(intersect(node_plates, parent_plates)) == 0) {
            nodes_to_aggregate <- c(nodes_to_aggregate, parent)
          }
        }
      }
      return(nodes_to_aggregate)
    },

    replicate_nodes = function(plates_reps) {
      parents_to_aggregate <- self$get_nodes_to_aggregate()
      nodes_to_remove <- list()
      new_nodes <- list()

      for (node in self$nodes) {
        if (length(node$plates) > 0) {
          nodes_to_remove <- c(nodes_to_remove, node)
          node_replicas <- lapply(1:plates_reps[[node$plates[[1]]]], function(x) node$clone(deep = TRUE))
          new_nodes <- c(new_nodes, node_replicas)
          for (i in seq_along(node_replicas)) {
            node_replicas[[i]]$name <- paste0(node$name, "_", i, "_")
          }
          if (node %in% parents_to_aggregate) {
            new_nodes <- c(new_nodes, Node$new(name = paste0(node$name, "_agg"), func = function(...) list(...), args = node_replicas, observed = FALSE))
          }
        }
      }
      self$nodes <- setdiff(c(self$nodes, new_nodes), nodes_to_remove)
      removed_nodes_names <- sapply(nodes_to_remove, function(x) x$name)
      return(removed_nodes_names)
    },

    update_plate_embedding = function() {
      plateDict <- list()
      plateDict[[1]] <- list(NULL, list())  # Equivalent to list(0 = list(NULL, list())) in the original intent
      idx <- 2  # Start index from 2 because 1 is used for the initial NULL plate
      labels <- character()

      for (node in self$nodes) {
        if (length(node$plates) > 0) {
          for (label in node$plates) {
            if (!(label %in% labels)) {
              plateDict[[idx]] <- list(label, list())
              labels <- c(labels, label)
              idx <- idx + 1
            }
            label_index <- which(sapply(plateDict, function(x) x[[1]] == label))
            plateDict[[label_index]][[2]] <- c(plateDict[[label_index]][[2]], node$name)
          }
        } else {
          plateDict[[1]][[2]] <- c(plateDict[[1]][[2]], node$name)
        }
      }

      self$plates <- plateDict
    },

    add_node = function(node) {
      if (!(node %in% self$nodes)) {
        self$nodes <- c(self$nodes, node)
      }
      self$update_topol_order()
    },

    get_selection = function() {
      check_for_selection <- Filter(function(item) inherits(item, "Selection"), self$nodes)
      if (length(check_for_selection) > 0) {
        return(which(sapply(self$nodes, identical, check_for_selection[[1]])))
      } else {
        return(NULL)
      }
    },

    get_stratify = function() {
      check_for_stratify <- Filter(function(item) inherits(item, "Stratify"), self$nodes)
      if (length(check_for_stratify) > 0) {
        return(which(sapply(self$nodes, identical, check_for_stratify[[1]])))
      } else {
        return(NULL)
      }
    },

    get_missing = function() {
      check_for_missing <- Filter(function(item) inherits(item, "Missing"), self$nodes)
      if (length(check_for_missing) > 0) {
        return(check_for_missing)
      } else {
        return(NULL)
      }
    },

    get_node_by_name = function(name) {
      if (!is.character(name)) {
        return(NULL)
      } else {
        node <- Filter(function(item) item$name == name, self$nodes)
        if (length(node) == 0) {
          message(name, " not found")
          return(NULL)
        } else {
          return(node[[1]])
        }
      }
    },

    update_adj_mat = function() {
      nodes_names <- sapply(self$nodes, function(node) node$name)
      matrix <- as.data.frame(matrix(0, nrow = length(self$nodes), ncol = length(self$nodes), dimnames = list(nodes_names, nodes_names)))

      for (node in self$nodes) {
        if (!is.null(node$parents)) {
          for (parent in node$parents) {
            matrix[node$name, parent$name] <- 1
          }
        }
      }
      self$adj_mat <- matrix
    },

    update_topol_order = function() {
      self$update_adj_mat()
      G <- igraph::graph_from_adjacency_matrix(as.matrix(self$adj_mat), mode = "directed")
      self$top_order <- names(igraph::topo_sort(G))
    },

    generate_dot = function() {
      shape_dict <- list(Node = "ellipse", Selection = "doublecircle", Stratify = "doubleoctagon", Missing = "Mcircle")
      dot_str <- "digraph G{\n"

      for (i in seq_along(self$nodes)) {
        if (self$nodes[[i]]$visible) {
          node_str <- sprintf('"%s" [shape=%s];\n', self$nodes[[i]]$name, shape_dict[[class(self$nodes[[i]])[1]]])
          dot_str <- paste0(dot_str, node_str)
        }
      }

      for (parent_node in colnames(self$adj_mat)) {
        if (self$get_node_by_name(parent_node)$visible) {
          for (node_idx in rownames(self$adj_mat)) {
            if (self$adj_mat[node_idx, parent_node] == 1 && self$get_node_by_name(node_idx)$visible) {
              tmp_str <- sprintf('"%s" -> "%s";\n', parent_node, node_idx)
              dot_str <- paste0(dot_str, tmp_str)
            }
          }
        }
      }

      if (length(self$plates) > 1) {
        dot_str <- paste0(dot_str, get_plate_dot(self$plates))  # Assuming you have an R equivalent for get_plate_dot
      }

      dot_str <- paste0(dot_str, "}")
      return(dot_str)
    },

    draw = function(folded = TRUE) {
      dot_str <- if (folded) self$folded_dot_str else self$unfolded_dot_str
      writeLines(dot_str, paste0(self$name, "_DOT.txt"))
      s <- igraph::graph_from_dot(dot_str)
      plot(s)  # Assumes an R function to plot Graphviz DOT files
    },

    simulate = function(num_samples, csv_name = "", output_path = "./", selection = TRUE, stratify = TRUE, missing = TRUE) {
      # Ensure output path ends with a slash
      if (substr(output_path, nchar(output_path), nchar(output_path)) != "/") {
        output_path <- paste0(output_path, "/")
      }

      # Start timing
      tic <- Sys.time()
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulation started.\n")

      # Traverse the graph to generate initial output
      output_dict <- self$traverse_graph(num_samples, output_path, missing, TRUE)

      # Handle selection bias if selection is TRUE
      selectionNode <- self$get_selection()
      if (selection && !is.null(selectionNode)) {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulating selection bias.\n")
        output_dict <- self$nodes[[selectionNode]]$filter_output(output_dict)

        while (length(output_dict[[1]]) < num_samples) {
          temp_output <- self$nodes[[selectionNode]]$filter_output(
            output_dict = self$traverse_graph(1, output_path, missing, FALSE)
          )
          output_dict <- mapply(c, output_dict, temp_output, SIMPLIFY = FALSE)
        }
      }

      # Filter the output_dict to only include observed nodes
      output_dict <- output_dict[sapply(names(output_dict), function(k) self$get_node_by_name(k)$observed)]

      # Prettify the output
      output_dict <- self$prettify_output(output_dict)

      # Handle stratification if stratify is TRUE
      stratifyNode <- self$get_stratify()
      if (stratify && !is.null(stratifyNode)) {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Stratifying the data.\n")
        output_dict <- self$nodes[[stratifyNode]]$filter_output(output_dict)
      }

      # Save to CSV if csv_name is provided
      if (csv_name != "") {
        if (grepl("\\.csv$", csv_name)) {
          csv_name <- sub("\\.csv$", "", csv_name)
        }
        if (!is.null(stratifyNode)) {
          lapply(names(output_dict), function(key) {
            write.csv(output_dict[[key]], file = paste0(output_path, csv_name, '_', key, '.csv'), row.names = FALSE)
          })
        } else {
          write.csv(as.data.frame(output_dict), file = paste0(output_path, csv_name, '.csv'), row.names = FALSE)
        }
      }

      # End timing
      toc <- Sys.time()
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulation finished in ", round(difftime(toc, tic, units = "secs"), 4), " seconds.\n")

      return(output_dict)
    },

    traverse_graph = function(num_samples, output_path, missing, show_log) {
      output_dict <- list()

      for (node_name in self$top_order) {
        node <- self$get_node_by_name(node_name)

        if (show_log && (inherits(node, "Missing") || inherits(node, "Node"))) {
          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulating node \"", node$name, "\".\n", sep = "")
        }

        node$node_simulate(num_samples, output_path)

        if (inherits(node, "Selection")) {
          stopifnot(all(sapply(node$output, is.logical)), "The selection node's function should return a boolean")
        } else if (inherits(node, "Missing") && missing) {
          stopifnot(all(sapply(node$index_node$output, is.logical)), "The index node's function should return a boolean")
          output_dict[[node$name]] <- node$output
        } else if (inherits(node, "Stratify")) {
          stopifnot(all(sapply(node$output, is.character)), "The stratification node's function should return a string")
        } else {
          output_dict[[node$name]] <- node$output
        }
      }

      return(output_dict)
    },

    prettify_output = function(output_dict) {
      keys_to_remove <- character()

      for (key in names(output_dict)) {
        node <- self$get_node_by_name(key)

        # Check if the node handles multiple columns
        if (node$handle_multi_cols) {
          node_output <- output_dict[[key]]
          keys_to_remove <- c(keys_to_remove, key)

          tryCatch({
            unfolded_output <- self$vec2dict(key, node_output)
            output_dict <- c(output_dict, unfolded_output)
          }, error = function(e) {
            if (inherits(e, "index out of bounds")) {
              stop("All vectors returned by ", node$func_name, " must be of the same length")
            } else {
              stop("The output of ", node$func_name, " either is not subscriptable or has no length method")
            }
          })
        } else if (!is.null(node$handle_multi_return)) {
          output_dict[[key]] <- lapply(node$output, node$handle_multi_return)
        }
      }

      # Remove keys that were processed
      output_dict[keys_to_remove] <- NULL

      return(output_dict)
    },

    vec2dict = function(key, node_output) {
      num_reps <- length(node_output[[1]])
      node_dict <- setNames(vector("list", num_reps), paste0(key, "_", seq_len(num_reps) - 1))

      for (sample in seq_along(node_output)) {
        for (rep in seq_len(num_reps)) {
          node_dict[[rep]] <- c(node_dict[[rep]], node_output[[sample]][[rep]])
        }
      }

      names(node_dict) <- paste0(key, "_", seq_len(num_reps) - 1)
      return(node_dict)
    },

    ml_simulation = function(num_samples, train_test_ratio, stratify = FALSE, include_external = FALSE, csv_prefix = "") {
      if (csv_prefix != "") {
        csv_prefix <- paste0(csv_prefix, "_")
      }

      num_tr_samples <- as.integer(num_samples * train_test_ratio)
      num_te_samples <- num_samples - num_tr_samples

      train_dict <- self$simulate(num_samples = num_tr_samples, stratify = stratify, csv_name = paste0(csv_prefix, "train"))
      test_dict <- self$simulate(num_samples = num_te_samples, stratify = stratify, csv_name = paste0(csv_prefix, "test"))

      output <- list(train_dict, test_dict)

      if (include_external) {
        exter_dict <- self$simulate(num_samples = num_te_samples, stratify = stratify, selection = FALSE, csv_name = paste0(csv_prefix, "external"))
        output <- c(output, list(exter_dict))
      }

      return(output)
    },

    update_nodes = function(removed_nodes) {
      # Update the constructors of the nodes to include the new parents
      for (child_name in self$top_order) {
        child <- self$get_node_by_name(child_name)

        for (parent in child$parents) {
          usage <- self$get_parent_usage(child, parent)

          if (parent$name %in% removed_nodes) {  # Avoid modifying nodes in plates with parents not in a plate
            if (identical(child$plates, parent$plates)) {  # TODO: Change when you allow for nested plates
              self$match_parents(child, parent, usage)
            } else {  # If child is in another plate, or not in a plate itfp
              self$assign_parent_to_child(child, parent, usage)
            }
          } else {
            if (!is.null(child$plates)) {
              self$assign_parent_to_child(child, self$get_node_by_name(parent$name), usage, agg = 0)
            }
          }
        }

        child$args <- child$parse_func_arguments()$args
        child$kwargs <- child$parse_func_arguments()$kwargs
        child$update_parents()
      }
    },

    get_parent_usage = function(child, parent) {
      if (parent %in% child$constructor$args) {
        ind <- which(sapply(child$constructor$args, identical, parent))
        tuple_usage <- list(ind, "arg")
      } else {
        key <- names(child$constructor$kwargs)[sapply(child$constructor$kwargs, identical, parent)]
        tuple_usage <- list(key, "kwarg")
      }

      if (is.null(tuple_usage)) {
        stop(parent$name, " is not a parent of ", child$name)
      }

      return(tuple_usage)
    },

    match_parents = function(child, parent, usage) {
      replica_index <- tail(strsplit(child$name, "_")[[1]], 2)[1]

      if (usage[[2]] == "arg") {
        child$constructor$args[[usage[[1]]]] <- self$get_node_by_name(paste0(parent$name, "_", replica_index, "_"))
      } else {  # if in kwargs
        child$constructor$kwargs[[usage[[1]]]] <- self$get_node_by_name(paste0(parent$name, "_", replica_index, "_"))
      }
    },

    assign_parent_to_child = function(child, parent, usage, agg = 1) {
      parent_name <- if (agg == 1) paste0(parent$name, "_agg") else parent$name

      if (usage[[2]] == "arg") {
        child$constructor$args[[usage[[1]]]] <- self$get_node_by_name(parent_name)
      } else {  # if in kwargs
        child$constructor$kwargs[[usage[[1]]]] <- self$get_node_by_name(parent_name)
      }
    }
  )
)
