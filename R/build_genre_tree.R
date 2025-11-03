build_genre_tree <- function(tags_long, platform_name, vote_weighted = TRUE) {
  basic_adjacency <- get_tag_cooccurrence_matrix(tags_long)

  final_adjacency <- if (vote_weighted) {
    apply_vote_weighting(basic_adjacency, tags_long)
  } else {
    basic_adjacency
  }

  initial_network <- create_igraph_from_matrix(final_adjacency)
  bidirectional_removed <- remove_weaker_direction(final_adjacency)
  tree_structure <- select_strongest_parent(bidirectional_removed)
  igraph_tree <- create_igraph_from_matrix(tree_structure)
  final_tree <- add_popmusic_as_metagerne(igraph_tree)

  save_results(
    initial_network,
    bidirectional_removed,
    final_tree,
    platform_name
  )
  final_tree
}

create_igraph_from_matrix <- function(adjacency_matrix) {
  igraph::graph_from_adjacency_matrix(
    adjacency_matrix,
    mode = "directed",
    weighted = TRUE
  )
}

add_popmusic_as_metagerne <- function(graph) {
  nodes_without_parents <- igraph::V(graph)[
    igraph::degree(graph, mode = "out") == 0
  ]$name

  if (!"POPULAR MUSIC" %in% igraph::V(graph)$name) {
    graph <- igraph::add_vertices(graph, 1, name = "POPULAR MUSIC")
  } else {
    warning(
      "Graph already contains a node 'POPULAR MUSIC'; can't add super genre."
    )
    return(graph)
  }
  for (node in nodes_without_parents) {
    graph <- igraph::add_edges(
      graph,
      c(node, "POPULAR MUSIC"),
      attr = list(weight = 0)
    )
  }
  graph
}

save_results <- function(
  initial_graph,
  bidirectional_removed,
  final_tree,
  platform_name
) {
  export_graph_for_gephi_import(
    initial_graph,
    sprintf("%s_initial_genre_network", platform_name)
  )
  genealogy_graph <- create_igraph_from_matrix(bidirectional_removed)
  # Add POPULAR MUSIC to genealogy graph and create proper ADG
  genealogy_graph <- add_popmusic_as_metagerne(genealogy_graph)
  genealogy_graph <- remove_cycles_preserve_tree(genealogy_graph, final_tree)

  export_graph_for_gephi_import(
    genealogy_graph,
    sprintf("%s_child_of_network", platform_name)
  )
  export_graph_for_gephi_import(
    final_tree,
    sprintf("%s_genre_tree", platform_name)
  )
  saveRDS(
    genealogy_graph,
    sprintf("models/trees/%s_genealogy.rds", platform_name)
  )
  saveRDS(final_tree, sprintf("models/trees/%s_graph.rds", platform_name))
}


get_tag_cooccurrence_matrix <- function(tags) {
  message("CALCULATE BASIC ADJACENCY MATRIX:")
  message("Counting appearance of individual tags...")
  tag_counts <- tags |> dplyr::count(.data$tag_name)

  message("Preparing combinations of tags...")
  tag_combinations <- get_within_track_combinations(tags)

  message("Calculating edge weights...")
  tag_names <- tag_counts$tag_name
  empty_matrix <- create_empty_adjacency_matrix(tag_names)

  if (nrow(tag_combinations) == 0) {
    return(empty_matrix)
  }

  cooccurrence_counts <- count_tag_cooccurrences(tag_combinations)
  tag_count_lookup <- create_tag_count_lookup(tag_counts)
  weighted_cooccurrences <- calculate_cooccurrence_weights(
    cooccurrence_counts,
    tag_count_lookup
  )

  populate_adjacency_matrix(empty_matrix, weighted_cooccurrences, tag_names)
}

create_empty_adjacency_matrix <- function(tag_names) {
  n_tags <- length(tag_names)
  matrix(0, nrow = n_tags, ncol = n_tags, dimnames = list(tag_names, tag_names))
}

count_tag_cooccurrences <- function(combinations) {
  stats::aggregate(
    list(count = rep(1, nrow(combinations))),
    by = list(tag_i = combinations$tag_name_i, tag_j = combinations$tag_name_j),
    FUN = sum
  )
}

create_tag_count_lookup <- function(tag_counts) {
  stats::setNames(tag_counts$n, tag_counts$tag_name)
}

calculate_cooccurrence_weights <- function(
  cooccurrence_counts,
  tag_count_lookup
) {
  cooccurrence_counts$weight <- cooccurrence_counts$count /
    tag_count_lookup[cooccurrence_counts$tag_i]
  cooccurrence_counts
}

populate_adjacency_matrix <- function(
  adjacency_matrix,
  weighted_cooccurrences,
  tag_names
) {
  coordinate_matrix <- create_coordinate_matrix(
    weighted_cooccurrences,
    tag_names
  )
  valid_coordinates <- find_valid_coordinates(coordinate_matrix)

  if (any(valid_coordinates)) {
    adjacency_matrix[coordinate_matrix[
      valid_coordinates,
    ]] <- weighted_cooccurrences$weight[valid_coordinates]
  }
  adjacency_matrix
}

create_coordinate_matrix <- function(weighted_cooccurrences, tag_names) {
  cbind(
    match(weighted_cooccurrences$tag_i, tag_names),
    match(weighted_cooccurrences$tag_j, tag_names)
  )
}

find_valid_coordinates <- function(coordinate_matrix) {
  !is.na(coordinate_matrix[, 1]) & !is.na(coordinate_matrix[, 2])
}


apply_vote_weighting <- function(basic_adjacency, tags) {
  message("APPLY VOTE WEIGHTING TO ADJACENCY MATRIX:")
  message("Preparing combinations of tags...")
  tag_combinations <- get_within_track_combinations(tags)

  if (nrow(tag_combinations) == 0) {
    return(basic_adjacency)
  }

  genres <- rownames(basic_adjacency)
  vote_weights <- create_empty_weight_matrix(genres)
  genre_lookup <- create_genre_index_lookup(genres)
  combinations_by_genre <- split_by_source_genre(tag_combinations)

  message("Calculating vote weights for all genres...")

  pb <- utils::txtProgressBar(min = 0, max = length(genres), style = 3)
  on.exit(close(pb), add = TRUE)
  for (i in seq_along(genres)) {
    current_genre <- genres[i]

    if (current_genre %in% names(combinations_by_genre)) {
      genre_combinations <- combinations_by_genre[[current_genre]]
      weight_calculations <- calculate_vote_based_weights(genre_combinations)

      if (nrow(weight_calculations) > 0) {
        assign_weights_to_matrix(
          vote_weights,
          weight_calculations,
          i,
          genre_lookup
        )
      }
    }
    utils::setTxtProgressBar(pb, i)
  }
  basic_adjacency * vote_weights
}

create_empty_weight_matrix <- function(genres) {
  n_genres <- length(genres)
  matrix(1, nrow = n_genres, ncol = n_genres, dimnames = list(genres, genres))
}

create_genre_index_lookup <- function(genres) {
  stats::setNames(seq_along(genres), genres)
}

split_by_source_genre <- function(combinations) {
  split(combinations, combinations$tag_name_i)
}

assign_weights_to_matrix <- function(
  weight_matrix,
  weight_calculations,
  row_index,
  genre_lookup
) {
  target_indices <- genre_lookup[weight_calculations$tag_name_j]
  valid_indices <- !is.na(target_indices)

  if (any(valid_indices)) {
    weight_matrix[
      row_index,
      target_indices[valid_indices]
    ] <- weight_calculations$weight[valid_indices]
  }
}

calculate_vote_based_weights <- function(genre_combinations) {
  if (nrow(genre_combinations) == 0) {
    return(data.frame(tag_name_j = character(0), weight = numeric(0)))
  }

  track_total_votes <- aggregate_votes_per_track(genre_combinations)
  enriched_combinations <- merge_track_totals(
    genre_combinations,
    track_total_votes
  )
  proportional_votes <- calculate_proportional_votes(enriched_combinations)
  aggregated_proportions <- aggregate_target_proportions(
    proportional_votes
  )
  appearance_counts <- count_target_genre_appearances(genre_combinations)

  final_weights <- merge_and_normalize_weights(
    aggregated_proportions,
    appearance_counts
  )
  final_weights[, c("tag_name_j", "weight")]
}

aggregate_votes_per_track <- function(combinations) {
  stats::aggregate(
    list(n_votes_wo_genre_i = combinations$tag_count_j),
    by = list(track.s.id = combinations$track.s.id),
    FUN = sum
  )
}

merge_track_totals <- function(combinations, track_totals) {
  merge(combinations, track_totals, by = "track.s.id", sort = FALSE)
}

calculate_proportional_votes <- function(combinations) {
  combinations$n_votes_total <- combinations$n_votes_wo_genre_i +
    combinations$tag_count_i
  combinations$prop_genre_j <- combinations$tag_count_j /
    combinations$n_votes_total
  combinations
}

aggregate_target_proportions <- function(proportional_votes) {
  stats::aggregate(
    list(nonnormal_weight_ij = proportional_votes$prop_genre_j),
    by = list(tag_name_j = proportional_votes$tag_name_j),
    FUN = sum
  )
}

count_target_genre_appearances <- function(combinations) {
  stats::aggregate(
    list(n = rep(1, nrow(combinations))),
    by = list(tag_name_j = combinations$tag_name_j),
    FUN = sum
  )
}

merge_and_normalize_weights <- function(
  aggregated_proportions,
  appearance_counts
) {
  weights <- merge(
    aggregated_proportions,
    appearance_counts,
    by = "tag_name_j",
    sort = FALSE
  )
  weights$weight <- weights$nonnormal_weight_ij / weights$n
  weights
}

get_within_track_combinations <- function(tags) {
  tags |>
    dplyr::inner_join(
      tags,
      by = "track.s.id",
      suffix = c("_i", "_j"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$tag_name_i != .data$tag_name_j)
}

remove_weaker_direction <- function(adjacency_matrix) {
  stronger_direction_mask <- adjacency_matrix >= t(adjacency_matrix)
  forward_differences <- adjacency_matrix - t(adjacency_matrix)
  backward_differences <- t(adjacency_matrix) - adjacency_matrix

  result <- ifelse(stronger_direction_mask, pmax(forward_differences, 0), 0) +
    ifelse(!t(stronger_direction_mask), pmax(backward_differences, 0), 0)

  rownames(result) <- rownames(adjacency_matrix)
  colnames(result) <- colnames(adjacency_matrix)
  result
}

select_strongest_parent <- function(hierarchy_matrix) {
  n_rows <- nrow(hierarchy_matrix)
  n_cols <- ncol(hierarchy_matrix)

  if (n_rows == 0 || n_cols == 0) {
    return(create_empty_matrix_with_names(hierarchy_matrix))
  }

  strongest_parent_indices <- apply(hierarchy_matrix, 1, which.max)
  node_indices <- seq_len(n_rows)
  coordinate_pairs <- cbind(node_indices, strongest_parent_indices)

  result <- matrix(0, nrow = n_rows, ncol = n_cols)
  result[coordinate_pairs] <- hierarchy_matrix[coordinate_pairs]

  rownames(result) <- rownames(hierarchy_matrix)
  colnames(result) <- colnames(hierarchy_matrix)
  result
}

create_empty_matrix_with_names <- function(template_matrix) {
  matrix(
    0,
    nrow = nrow(template_matrix),
    ncol = ncol(template_matrix),
    dimnames = list(rownames(template_matrix), colnames(template_matrix))
  )
}

#' Remove cycles from a graph while preserving all tree edges
#'
#' Iteratively removes the weakest edge in each cycle until the graph is acyclic.
#' Guarantees that all edges from the tree parameter are preserved.
#'
#' @param graph An igraph object that may contain cycles
#' @param tree An igraph object representing the tree (subgraph to preserve)
#'
#' @return An acyclic directed graph (ADG) containing all tree edges
#'
#' @keywords internal
remove_cycles_preserve_tree <- function(graph, tree) {
  # Get tree edges to protect them
  tree_edges_df <- igraph::as_data_frame(tree, what = "edges")
  tree_edge_keys <- paste(tree_edges_df$from, tree_edges_df$to, sep = "->")

  # Work with a copy
  g <- graph

  # Iteratively remove cycles
  max_iterations <- igraph::ecount(g) # Safety limit
  iteration <- 0

  while (!igraph::is_dag(g) && iteration < max_iterations) {
    iteration <- iteration + 1

    # Find a cycle using topological sort attempt
    # When there's a cycle, topo_sort will fail, but we can detect cycles
    # by checking if any strongly connected component has size > 1

    # Simple approach: try to find back edges
    # A back edge creates a cycle in DFS
    edges_df <- igraph::as_data_frame(g, what = "edges")

    if (nrow(edges_df) == 0) {
      break
    }

    # Try topological sort - if it fails, there's a cycle
    tryCatch(
      {
        igraph::topo_sort(g)
        break # Success - no cycles
      },
      error = function(e) {
        # There's a cycle, find and remove weakest non-tree edge

        # Find edges that are not in the tree
        edge_keys <- paste(edges_df$from, edges_df$to, sep = "->")
        is_tree_edge <- edge_keys %in% tree_edge_keys

        # Get weights (use 0 if no weight attribute)
        weights <- if ("weight" %in% colnames(edges_df)) {
          edges_df$weight
        } else {
          rep(1, nrow(edges_df))
        }

        # Find non-tree edges
        non_tree_indices <- which(!is_tree_edge)

        if (length(non_tree_indices) == 0) {
          # This shouldn't happen - tree should be acyclic
          warning(
            "Cycle detected but only tree edges remain. ",
            "Tree may not be acyclic."
          )
          break
        }

        # Among non-tree edges, find the weakest
        non_tree_weights <- weights[non_tree_indices]
        weakest_idx <- non_tree_indices[which.min(non_tree_weights)]

        # Remove this edge
        edge_to_remove <- igraph::E(g)[weakest_idx]
        g <<- igraph::delete_edges(g, edge_to_remove)
      }
    )
  }

  if (iteration >= max_iterations) {
    warning(
      "Maximum iterations reached in cycle removal. ",
      "Graph may still contain cycles."
    )
  }

  g
}
