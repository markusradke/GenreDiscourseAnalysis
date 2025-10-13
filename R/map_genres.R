# Optimized version: Uses caching and vectorized operations
get_initial_genre_mapping <- function(tags, graph) {
  # Clear cache at start for fresh run
  clear_subgraph_cache()

  # Filter tags that exist in graph (vectorized)
  graph_node_names <- igraph::V(graph)$name
  tags_in_graph <- tags |>
    dplyr::filter(.data$tag_name %in% graph_node_names)

  if (nrow(tags_in_graph > 0)) {
    # Process represented tracks
    initial_genres <- get_initial_genres_tree_and_votes_based(
      tags_in_graph,
      graph
    )
  } else {
    initial_genres <- data.frame(
      track.s.id = character(0),
      track.s.title = character(0),
      track.s.firstartist.name = character(0),
      initial_genre = logical(0)
    )
  }
  # Handle unrepresented tracks efficiently
  all_track_ids <- unique(tags$track.s.id)
  represented_track_ids <- unique(initial_genres$track.s.id)
  unrepresented_ids <- setdiff(all_track_ids, represented_track_ids)

  if (length(unrepresented_ids) > 0) {
    unrepresented_tracks <- tags |>
      dplyr::filter(.data$track.s.id %in% unrepresented_ids) |>
      dplyr::select(
        "track.s.id",
        "track.s.title",
        "track.s.firstartist.name"
      ) |>
      dplyr::mutate(
        initial_genre = NA,
        track.s.id = as.character(track.s.id)
      ) |>
      dplyr::distinct()

    # Use dplyr::bind_rows instead of rbind for better performance
    result <- dplyr::bind_rows(initial_genres, unrepresented_tracks)
  } else {
    result <- initial_genres
  }

  # Clear cache to free memory
  clear_subgraph_cache()

  result |> dplyr::arrange(.data$track.s.id)
}


get_initial_genres_tree_and_votes_based <- function(tags, graph) {
  # determines the initial genre for each track by following
  # the tree branch with most votes as far as possible
  # votes are summed over all tags in the subtree of a tag
  # tie breaker: take the tag with lower total votes (more specific)

  # Pre-compute total votes for all genres (vectorized)
  total_votes_genres <- tags |>
    dplyr::group_by(.data$tag_name) |>
    dplyr::summarize(votes_total = sum(tag_count), .groups = "drop")

  tags <- tags |> dplyr::inner_join(total_votes_genres, by = "tag_name")

  # Pre-compute subtree votes for all unique tags (major optimization)
  message("Pre-computing subtree votes for optimization...")
  subtree_votes_cache <- compute_all_subtree_votes(
    unique(tags$tag_name),
    graph,
    tags
  )
  tags <- tags |> dplyr::left_join(subtree_votes_cache, by = "tag_name")

  # Split tags by track for efficient processing (avoid repeated filtering)
  tracks_info <- dplyr::distinct(
    tags,
    .data$track.s.id,
    .data$track.s.title,
    .data$track.s.firstartist.name
  ) |>
    dplyr::mutate(track.s.id = as.character(.data$track.s.id))
  tags_by_track <- split(tags, tags$track.s.id)
  track_ids <- names(tags_by_track)
  n_tracks <- length(track_ids)

  message(sprintf("Processing %d tracks...", n_tracks))

  # Process in batches to manage memory and provide progress updates
  batch_size <- 1000
  n_batches <- ceiling(n_tracks / batch_size)

  results_list <- vector("list", n_batches)

  for (batch_idx in seq_len(n_batches)) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_tracks)
    batch_track_ids <- track_ids[start_idx:end_idx]

    if (batch_idx %% 10 == 1 || batch_idx == n_batches) {
      message(sprintf(
        "Processing batch %d/%d (tracks %d-%d)",
        batch_idx,
        n_batches,
        start_idx,
        end_idx
      ))
    }

    # Process batch using lapply (much faster than loop + rbind)
    batch_results <- lapply(batch_track_ids, function(track_id) {
      track_tags <- tags_by_track[[track_id]]
      initial_genre <- get_tree_and_votes_based_mapping_optimized(
        track_tags,
        graph
      )

      data.frame(
        track.s.id = track_id,
        initial_genre = initial_genre,
        stringsAsFactors = FALSE
      )
    })

    results_list[[batch_idx]] <- dplyr::bind_rows(batch_results)
  }

  # Combine all results efficiently
  result <- dplyr::bind_rows(results_list) |>
    dplyr::inner_join(
      tracks_info,
      by = "track.s.id"
    )

  result
}

# Cache for subgraph results to avoid repeated calculations
.subgraph_cache <- new.env(hash = TRUE)

# Pre-compute subtree votes for all tags to avoid repeated calculations
compute_all_subtree_votes <- function(tag_names, graph, tags) {
  message(sprintf(
    "Computing subtree votes for %d unique tags...",
    length(tag_names)
  ))

  # Use lapply for vectorized processing
  subtree_votes_list <- lapply(tag_names, function(tag_name) {
    votes <- get_subtree_votes_cached(tag_name, graph, tags)
    data.frame(
      tag_name = tag_name,
      votes_sub = votes
    )
  })

  dplyr::bind_rows(subtree_votes_list)
}

# Cached version of get_subtree_votes
get_subtree_votes_cached <- function(root, graph, tags) {
  cache_key <- paste0("subgraph_", root)

  if (exists(cache_key, envir = .subgraph_cache)) {
    tags_subtree <- get(cache_key, envir = .subgraph_cache)
  } else {
    subgraph <- get_subgraph(graph, root)
    tags_subtree <- igraph::V(subgraph)$name
    assign(cache_key, tags_subtree, envir = .subgraph_cache)
  }

  # Use vectorized operations for better performance
  sum(tags$tag_count[tags$tag_name %in% tags_subtree], na.rm = TRUE)
}

# Optimized version that uses pre-computed subtree votes
get_tree_and_votes_based_mapping_optimized <- function(track_tags, graph) {
  if (nrow(track_tags) == 1) {
    return(track_tags$tag_name[1])
  }

  current_tags <- track_tags
  genre <- NA

  while (nrow(current_tags) > 1) {
    # Use pre-computed votes_sub instead of calculating on the fly
    most_voted <- current_tags |>
      dplyr::arrange(dplyr::desc(.data$votes_sub), .data$votes_total) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$tag_name)

    # Get children using cached subgraph
    cache_key <- paste0("subgraph_", most_voted)
    if (exists(cache_key, envir = .subgraph_cache)) {
      children <- get(cache_key, envir = .subgraph_cache)
    } else {
      subgraph <- get_subgraph(graph, most_voted)
      children <- igraph::V(subgraph)$name
      assign(cache_key, children, envir = .subgraph_cache)
    }

    children <- children[children != most_voted]
    current_tags <- current_tags |> dplyr::filter(.data$tag_name %in% children)

    if (nrow(current_tags) == 0) {
      genre <- most_voted
      break
    }
    if (nrow(current_tags) == 1) {
      genre <- current_tags |> dplyr::pull(.data$tag_name)
      break
    }
  }

  if (is.na(genre) && nrow(track_tags) > 0) {
    genre <- track_tags$tag_name[1]
  }

  return(genre)
}

# Function to clear cache (useful for testing or memory management)
clear_subgraph_cache <- function() {
  rm(list = ls(envir = .subgraph_cache), envir = .subgraph_cache)
}

get_tree_and_votes_based_mapping <- function(track_tags, graph, tags) {
  if (nrow(track_tags) == 1) {
    return(track_tags$tag_name)
  }
  while (nrow(track_tags) > 1) {
    most_voted <- track_tags |>
      dplyr::mutate(
        votes_sub = lapply(tag_name, get_subtree_votes, graph, tags) |>
          as.integer()
      ) |>
      dplyr::arrange(-.data$votes_sub, .data$votes_total) |> # todo modify to use votes in subtree
      dplyr::first() |>
      dplyr::pull(.data$tag_name)
    children <- get_subgraph(graph, most_voted) |>
      igraph::V() |>
      names()
    children <- children[children != most_voted]
    track_tags <- track_tags |> dplyr::filter(.data$tag_name %in% children)
    if (nrow(track_tags) == 0) {
      genre <- most_voted
    }
    if (nrow(track_tags) == 1) {
      genre <- track_tags |> dplyr::pull(.data$tag_name)
    }
  }
  genre
}

get_subtree_votes <- function(root, graph, tags) {
  subgraph <- get_subgraph(graph, root)
  tags_subtree <- igraph::V(subgraph)$name
  sum(
    tags$tag_count[tags$tag_name %in% tags_subtree],
    na.rm = TRUE
  )
}
