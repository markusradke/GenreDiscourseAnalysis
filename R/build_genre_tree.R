build_genre_tree <- function(tags_long, plattformname, vote_weighted = TRUE) {
  adjacency <- make_directed_adjacency_matrix(tags_long)
  if (vote_weighted) {
    adjacency <- get_vote_weighted_adjacency(adjacency, tags_long)
  }
  initial_graph <- igraph::graph_from_adjacency_matrix(
    adjacency,
    mode = "directed",
    weighted = TRUE
  )
  soft_child_of <- create_soft_child_of_network(adjacency)
  graph <- igraph::graph_from_adjacency_matrix(
    soft_child_of,
    mode = "directed",
    weighted = TRUE
  )
  democratic_tree <- get_democratic_parents(soft_child_of)
  graph <- igraph::graph_from_adjacency_matrix(
    democratic_tree,
    mode = "directed",
    weighted = TRUE
  )
  graph_connected <- igraph::largest_component(graph)
  unconnected_tags <- get_unconnected_tags(graph_connected, graph)

  export_graph_for_gephi_import(
    initial_graph,
    sprintf("%s_initial_genre_network", plattformname)
  )
  export_graph_for_gephi_import(
    graph_connected,
    sprintf("%s_genre_tree", plattformname)
  )
  saveRDS(graph_connected, "models/graph.rds")
  saveRDS(unconnected_tags, "models/unconnected_tags.rds")
}


make_directed_adjacency_matrix <- function(tags) {
  message("Counting appearance of individual tags...")
  denominator_tags <- tags |> dplyr::count(.data$tag_name)
  message("Preparing combinations of tags...")
  combinations <- make_combinations_frame(tags)
  combination_table <- table(combinations$tag_name_i, combinations$tag_name_j)

  message("Calculating edge weights...")
  adjacency <- initialize_adjacency_matrix(denominator_tags$tag_name)
  for (i in seq_along(denominator_tags$tag_name)) {
    current_tag <- denominator_tags$tag_name[i]
    if (current_tag %in% rownames(combination_table)) {
      weights <- combination_table[current_tag, ] / denominator_tags$n[i]
      adjacency[i, names(weights)] <- weights
    }
  }
  adjacency
}

initialize_adjacency_matrix <- function(mb_tags_unique) {
  adjacency <- matrix(
    0,
    nrow = length(mb_tags_unique),
    ncol = length(mb_tags_unique)
  )
  rownames(adjacency) <- mb_tags_unique
  colnames(adjacency) <- mb_tags_unique
  adjacency
}

get_vote_weighted_adjacency <- function(adjacency, tags) {
  message("Preparing combinations of tags...")
  combinations <- make_combinations_frame(tags)
  genres <- rownames(adjacency)
  weights <- matrix(0, nrow = nrow(adjacency), ncol = ncol(adjacency))
  rownames(weights) <- genres
  colnames(weights) <- genres
  for (i in seq_along(genres)) {
    message(sprintf("Caluclating genre %d of %d", i, length(genres)))
    genre <- genres[i]
    genre_i_only <- combinations |> dplyr::filter(.data$tag_name_i == genre)
    weigths_frame <- get_weights_genres_j_from_i(genre_i_only)
    weights_vec <- get_weights_vector_from_frame(weigths_frame, genres)
    weights[i, ] <- weights_vec
  }
  updated <- adjacency * weights
  updated
}

get_weights_vector_from_frame <- function(weights, genres) {
  lookup <- weights$weight
  names(lookup) <- weights$tag_name_j
  weights_vec <- lookup[genres]
  names(weights_vec) <- genres
  weights_vec[is.na(weights_vec)] <- 0
  weights_vec
}

get_weights_genres_j_from_i <- function(genre_i_only) {
  sum_votes_per_track_wo_genre_i <- genre_i_only |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::summarize(n_votes_wo_genre_i = sum(.data$tag_count_j))
  nonnormalized_weights_ij <- suppressMessages(
    genre_i_only |>
      dplyr::left_join(sum_votes_per_track_wo_genre_i, by = "track.s.id") |>
      dplyr::mutate(
        n_votes_total = .data$n_votes_wo_genre_i + .data$tag_count_i,
        prop_genre_j = .data$tag_count_j / .data$n_votes_total
      )
  ) |>
    dplyr::group_by(.data$tag_name_j) |>
    dplyr::summarize(nonnormal_weight_ij = sum(.data$prop_genre_j))
  n_appearances_genre_j <- genre_i_only |>
    dplyr::count(.data$tag_name_j)
  weights <- nonnormalized_weights_ij |>
    dplyr::left_join(n_appearances_genre_j, by = "tag_name_j") |>
    dplyr::mutate(weight = .data$nonnormal_weight_ij / .data$n)
  weights
}

make_combinations_frame <- function(tags) {
  tags |>
    dplyr::inner_join(
      tags,
      by = "track.s.id",
      suffix = c("_i", "_j"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$tag_name_i != .data$tag_name_j)
}


create_soft_child_of_network <- function(adjacency) {
  updated <- adjacency
  for (i in seq_len(nrow(adjacency))) {
    for (j in seq_len(ncol(adjacency))) {
      if (adjacency[i, j] >= adjacency[j, i]) {
        updated[j, i] <- 0
        updated[i, j] <- adjacency[i, j] - adjacency[j, i]
      } else {
        updated[i, j] <- 0
        updated[j, i] <- adjacency[j, i] - adjacency[i, j]
      }
    }
  }
  updated
}

get_democratic_parents <- function(child_of) {
  updated <- matrix(data = 0, nrow(child_of), ncol(child_of))
  rownames(updated) <- rownames(child_of)
  colnames(updated) <- colnames(child_of)
  for (i in seq_len(nrow(child_of))) {
    j <- which.max(child_of[i, ])
    updated[i, j] <- child_of[i, j]
  }
  updated
}

get_unconnected_tags <- function(graph_connected, graph_unconnected) {
  tags_connected <- igraph::V(graph_connected) |> names()
  tags_all <- igraph::V(graph_unconnected) |> names()
  unconnected_tags <- tags_all[!tags_all %in% tags_connected]
  unconnected_tags
}
