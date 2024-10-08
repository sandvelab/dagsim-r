Graph <- R6::R6Class(
  "Graph",
  public = list(
    name = NULL,
    nodes = NULL,
    plates = NULL,
    adj_matrix = NULL,
    topol_order = NULL,
    folded_dot_str = NULL,
    unfolded_dot_str = NULL,

    initialize = function(list_nodes, name = "Graph", plates_reps = NULL) {
      self$check_args(list_nodes)
      self$name <- name
      self$nodes <- list_nodes
      self$plates <- list()
      self$adj_matrix <- data.frame()
      self$topol_order <- character()
      self$update_topol_order()
      self$update_plate_embedding()
      self$folded_dot_str <- self$generate_dot()
      if (!is.null(plates_reps)) {
        self$unfolded_dot_str <- ""
        self$unfold_graph(plates_reps)
      }
    },

    #TO DO: check plates related
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

    get_node_by_class = function(class_name) {
      check_for_node <- Filter(function(item) inherits(item, class_name), self$nodes)
      if (length(check_for_node) > 0) {
        return(which(sapply(self$nodes, identical, check_for_node[[1]])))
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
          message("Node ", name, " not found.")
          return(NULL)
        } else {
          return(node[[1]])
        }
      }
    },

    update_adj_matrix = function() {
      nodes_names <- sapply(self$nodes, function(node) node$name)
      matrix <- as.data.frame(matrix(0, nrow = length(self$nodes), ncol = length(self$nodes), dimnames = list(nodes_names, nodes_names)))

      for (node in self$nodes) {
        for (parent in node$parents) {
          matrix[parent$name, node$name] <- 1
        }
      }
      self$adj_matrix <- matrix
    },

    update_topol_order = function() {
      self$update_adj_matrix()
      igraph_obj <- igraph::graph_from_adjacency_matrix(as.matrix(self$adj_matrix), mode = "directed")
      self$topol_order <- names(igraph::topo_sort(igraph_obj))
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

      for (parent_node in rownames(self$adj_matrix)) {
        if (self$get_node_by_name(parent_node)$visible) {
          for (child_node in colnames(self$adj_matrix)) {
            if (self$adj_matrix[parent_node, child_node] == 1 && self$get_node_by_name(child_node)$visible) {
              relation_str <- sprintf('"%s" -> "%s";\n', parent_node, child_node)
              dot_str <- paste0(dot_str, relation_str)
            }
          }
        }
      }

      #TO DO: Add when plates are supported
      #if (length(self$plates) > 1) {
      #  dot_str <- paste0(dot_str, get_plate_dot(self$plates))  # Assuming you have an R equivalent for get_plate_dot
      #}

      dot_str <- paste0(dot_str, "}")
      return(dot_str)
    },

    #TO DO: unfolded_dot_str is not yet supported. Add it when u add plates support.
    #TO DO: add option to save only when needed and support different output paths
    draw = function(folded = TRUE) {
      dot_str <- if (folded) self$folded_dot_str else self$unfolded_dot_str
      writeLines(dot_str, paste0(self$name, ".txt"))
      graphviz_obj <- DiagrammeR::grViz(dot_str)
      print(graphviz_obj)
      svg_obj <- DiagrammeRsvg::export_svg(graphviz_obj)
      rsvg::rsvg_png(charToRaw(svg_obj), file = paste0(self$name, ".png"))
    },

    #TO DO: ensure correct execution of Selection, Missing, Stratify nodes
    traverse_graph = function(num_samples, missing, show_log) {
      output_dict <- list()

      for (node_name in self$topol_order) {
        node <- self$get_node_by_name(node_name)

        if (show_log) {
          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulating node \"", node$name, "\".\n", sep = "")
        }

        node$node_simulate(num_samples)

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

    #TO DO: ensure correct execution of Selection, Missing, Stratify nodes
    #TO DO: handle arrays
    simulate = function(num_samples, save = FALSE, csv_name = "", output_path = "./", selection = TRUE, stratify = TRUE, missing = TRUE) {
      # Start timing
      tic <- Sys.time()
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulation started.\n")

      # Traverse the graph to generate initial output
      output_dict <- self$traverse_graph(num_samples, missing, TRUE)

      # TO DO: Handle selection node
      #selectionNode <- self$get_node_by_class("Selection")
      #if (selection && !is.null(selectionNode)) {
      #  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulating selection bias.\n")
      #  output_dict <- self$nodes[[selectionNode]]$filter_output(output_dict)
      #
      #  while (length(output_dict[[1]]) < num_samples) {
      #    temp_output <- self$nodes[[selectionNode]]$filter_output(
      #      output_dict = self$traverse_graph(1, missing, FALSE)
      #    )
      #    output_dict <- mapply(c, output_dict, temp_output, SIMPLIFY = FALSE)
      #  }
      #}

      # Filter the output_dict to only include observed nodes
      output_dict <- output_dict[sapply(names(output_dict), function(k) self$get_node_by_name(k)$observed)]

      # Prettify the output
      output_dict <- self$prettify_output(output_dict)

      # TO DO: Handle stratification if stratify is TRUE
      #stratifyNode <- self$get_node_by_class("Stratify")
      #if (stratify && !is.null(stratifyNode)) {
      #  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Stratifying the data.\n")
      #  output_dict <- self$nodes[[stratifyNode]]$filter_output(output_dict)
      #}

      if (save) {
        if (csv_name == "") {
          csv_name <- paste0(self$name, ".csv")
        } else {
          if (!grepl("\\.csv$", csv_name)) {
            csv_name <- paste0(csv_name, ".csv")
          }
        }

        if (!grepl("/$", output_path)) {
          output_path <- paste0(output_path, "/")
        }

        #TO DO: fix when we have stratification
        #if (!is.null(stratifyNode)) {
        #  lapply(names(output_dict), function(key) {
        #    write.csv(output_dict[[key]], file = paste0(output_path, sub("\\.csv$", "", csv_name), '_', key, '.csv'), row.names = FALSE)
        #  })
        #} else {
          output_df <- data.frame(matrix(unlist(output_dict), nrow=length(output_dict[[1]]), byrow=FALSE))
          colnames(output_df) <- names(output_dict)
          write.csv(output_df, file = paste0(output_path, csv_name), row.names = FALSE)
        # }
      }

      print(output_dict)
      # End timing
      toc <- Sys.time()
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": Simulation finished in ", round(difftime(toc, tic, units = "secs"), 4), " seconds.\n")

      return(output_dict)
    },

    #TO DO: handle arrays
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

    #TO DO: handle arrays
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

    #TO DO: check plates related
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

    #TO DO: check plates related
    match_parents = function(child, parent, usage) {
      replica_index <- tail(strsplit(child$name, "_")[[1]], 2)[1]

      if (usage[[2]] == "arg") {
        child$constructor$args[[usage[[1]]]] <- self$get_node_by_name(paste0(parent$name, "_", replica_index, "_"))
      } else {  # if in kwargs
        child$constructor$kwargs[[usage[[1]]]] <- self$get_node_by_name(paste0(parent$name, "_", replica_index, "_"))
      }
    },

    #TO DO: check plates related
    assign_parent_to_child = function(child, parent, usage, agg = 1) {
      parent_name <- if (agg == 1) paste0(parent$name, "_agg") else parent$name

      if (usage[[2]] == "arg") {
        child$constructor$args[[usage[[1]]]] <- self$get_node_by_name(parent_name)
      } else {  # if in kwargs
        child$constructor$kwargs[[usage[[1]]]] <- self$get_node_by_name(parent_name)
      }
    },

    #TO DO: check plates related
    update_nodes = function(removed_nodes) {
      # Update the constructors of the nodes to include the new parents
      for (child_name in self$topol_order) {
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

    #TO DO: check plates related
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

    #TO DO: check plates related
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

    #TO DO: check plates related
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
    }
  )
)
