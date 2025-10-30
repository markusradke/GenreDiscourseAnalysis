# Helper functions for testing algorithm correctness on large datasets

#' Create a stratified sample from a large dataset for testing
#'
#' @param initial_genres Full dataset
#' @param sample_size Number of rows to include in sample
#' @param preserve_rare_genres If TRUE, ensures rare genres are represented
#' @return Stratified sample that preserves genre distribution characteristics
create_stratified_sample <- function(
  long,
  sample_size = 1000,
  preserve_rare_genres = TRUE
) {
  set.seed(42) # For reproducibility

  # Convert long (track.s.id, tag_name, tag_count) to per-track initial_genres
  initial_genres <- long |>
    dplyr::group_by(track.s.id, tag_name) |>
    dplyr::summarise(
      total_tag_count = sum(tag_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(track.s.id) |>
    dplyr::slice_max(total_tag_count, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(id = track.s.id, initial_genre = tag_name)

  # Calculate genre frequencies
  genre_counts <- table(initial_genres$initial_genre)

  if (preserve_rare_genres) {
    # Ensure each genre has at least a minimum representation
    min_per_genre <- max(1, sample_size %/% (length(genre_counts) * 10))

    # Start with minimum samples for each genre
    stratified_sample <- initial_genres[0, ] # Empty dataframe with same structure

    for (genre in names(genre_counts)) {
      genre_data <- initial_genres[initial_genres$initial_genre == genre, ]
      n_take <- min(nrow(genre_data), min_per_genre)
      if (n_take > 0) {
        sample_indices <- sample(nrow(genre_data), n_take)
        stratified_sample <- rbind(
          stratified_sample,
          genre_data[sample_indices, ]
        )
      }
    }

    # Fill remaining slots proportionally
    remaining_slots <- sample_size - nrow(stratified_sample)
    if (remaining_slots > 0) {
      remaining_data <- initial_genres[
        !initial_genres$id %in% stratified_sample$id,
      ]
      if (nrow(remaining_data) > 0) {
        remaining_indices <- sample(
          nrow(remaining_data),
          min(remaining_slots, nrow(remaining_data))
        )
        stratified_sample <- rbind(
          stratified_sample,
          remaining_data[remaining_indices, ]
        )
      }
    }
  } else {
    # Simple random sample
    sample_indices <- sample(
      nrow(initial_genres),
      min(sample_size, nrow(initial_genres))
    )
    stratified_sample <- initial_genres[sample_indices, ]
  }

  # Reset IDs to be sequential
  stratified_sample$id <- seq_len(nrow(stratified_sample))

  return(stratified_sample)
}

#' Test algorithm consistency across different sample sizes
#'
#' @param initial_genres Full dataset
#' @param graph Genre hierarchy graph
#' @param min_n Minimum songs per genre
#' @param root_genre Root genre name
#' @param sample_sizes Vector of sample sizes to test
#' @return List of results for each sample size
test_algorithm_scaling <- function(
  long,
  graph,
  min_n,
  root_genre,
  sample_sizes = c(100, 500, 1000, 2000)
) {
  results <- list()

  for (sample_size in sample_sizes) {
    cat("Testing with sample size:", sample_size, "\n")

    sample_data <- create_stratified_sample(long, sample_size)

    # Run algorithm (fold_genre_tree_bottom_to_top expects an initial_genres-shaped DF)
    result <- fold_genre_tree_bottom_to_top(
      sample_data,
      graph,
      min_n,
      root_genre
    )
    results[[as.character(sample_size)]] <- list(
      sample_size = sample_size,
      n_metagenres = nrow(result$n_songs),
      metagenres = result$n_songs$genre,
      parent_violations = check_parent_violations(result),
      tree_properties = check_tree_properties(result)
    )
  }

  return(results)
}

#' Check for parent-child relationship violations
#'
#' @param result Result from fold_genre_tree_bottom_to_top
#' @return List with violation details
check_parent_violations <- function(result) {
  violations <- list()

  metagenres <- result$n_songs$genre
  metagenre_graph <- result$metagenre_graph

  for (metagenre in metagenres) {
    if (metagenre != "root") {
      parent_vertices <- igraph::neighbors(
        metagenre_graph,
        metagenre,
        mode = "out"
      )
      if (length(parent_vertices) > 0) {
        parent <- names(parent_vertices)[1]
        if (!parent %in% metagenres && parent != "root") {
          violations[[length(violations) + 1]] <- list(
            child = metagenre,
            parent = parent,
            available_metagenres = metagenres
          )
        }
      }
    }
  }

  return(violations)
}

#' Check basic tree properties
#'
#' @param result Result from fold_genre_tree_bottom_to_top
#' @return List with tree property checks
check_tree_properties <- function(result) {
  metagenre_graph <- result$metagenre_graph

  n_vertices <- igraph::vcount(metagenre_graph)
  n_edges <- igraph::ecount(metagenre_graph)

  return(list(
    is_tree = (n_edges == n_vertices - 1),
    is_connected = igraph::is_connected(metagenre_graph, mode = "weak"),
    n_vertices = n_vertices,
    n_edges = n_edges,
    has_root = "root" %in% igraph::V(metagenre_graph)$name
  ))
}

#' Create a problematic test case based on known failure patterns
#'
#' @param failure_pattern Character describing the type of failure
#' @return Test dataset that might trigger the issue
create_problematic_test_case <- function(failure_pattern = "parent_violation") {
  if (failure_pattern == "parent_violation") {
    # Create a scenario where intermediate nodes might get excluded
    edges <- data.frame(
      from = c(
        # Many leaf nodes pointing to intermediate nodes
        paste0("leaf_", 1:20),
        # Intermediate nodes pointing to higher levels
        "intermediate_a",
        "intermediate_b",
        "intermediate_c",
        # Higher levels pointing to root
        "higher_a",
        "higher_b"
      ),
      to = c(
        rep(
          c("intermediate_a", "intermediate_b", "intermediate_c"),
          length.out = 20
        ),
        "higher_a",
        "higher_a",
        "higher_b",
        "root",
        "root"
      )
    )

    graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

    # Create data where some intermediates might have few songs
    # but their children have many
    initial_genres <- data.frame(
      id = 1:500,
      initial_genre = c(
        rep("leaf_1", 80), # Lots in one leaf
        rep("leaf_2", 70), # Lots in another
        rep("leaf_3", 60), # Decent amount
        rep("intermediate_a", 5), # Very few direct songs in intermediate
        rep("intermediate_b", 3), # Very few direct songs in intermediate
        sample(paste0("leaf_", 4:20), 282, replace = TRUE) # Rest scattered
      ),
      root = rep("root", 500)
    )

    # Convert to long format (one tag per track, tag_count = 1)
    long <- data.frame(
      track.s.id = initial_genres$id,
      tag_name = initial_genres$initial_genre,
      tag_count = 1
    )

    return(list(long = long, graph = graph))
  }

  # Add other failure patterns as needed
  stop("Unknown failure pattern: ", failure_pattern)
}
