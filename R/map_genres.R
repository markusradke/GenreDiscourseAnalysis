# todo write tests
# todo test if it works for larger datasets
# todo optimize: This takes very long for large datasets
get_initial_genre_mapping <- function(tags, graph, platform) {
  tags_in_graph <- tags |>
    dplyr::filter(tag_name %in% igraph::V(graph)$name)
  if (platform == "MusicBrainz") {
    initial_genres <- get_initial_genres_tree_and_votes_based(
      tags_in_graph,
      graph
    )
  } else if (platform == "Discogs" || platform == "Spotify") {
    initial_genres <- get_initial_genres_most_detailed(tags_in_graph, graph)
  } else {
    stop("Unknown platform")
  }
  unrepresented_ids <- setdiff(
    unique(tags$track.s.id),
    initial_genres$track.s.id
  )
  unrepresented_tracks <- data.frame(
    track.s.id = unrepresented_ids,
    initial_genre = rep(NA, length(unrepresented_ids))
  )
  rbind(initial_genres, unrepresented_tracks) |>
    dplyr::arrange(.data$track.s.id)
}

get_initial_genres_most_detailed <- function(tags, graph) {
  # returns the genres furthest away from the root with least votes
  distances_to_root <- get_distances_to_root(graph)
  n_tag <- tags |>
    dplyr::group_by(.data$tag_name) |>
    dplyr::summarize(tag_count = sum(.data$tag_count), .groups = "drop")
  tags_incl_count <- dplyr::select(tags, -"tag_count") |>
    dplyr::inner_join(n_tag, by = "tag_name")
  initial_metagenres <- tags_incl_count |>
    dplyr::left_join(distances_to_root, by = "tag_name") |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::arrange(-.data$hierarchy_level, .data$tag_count) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select("track.s.id", initial_genre = "tag_name")
}

get_distances_to_root <- function(graph) {
  root <- get_graph_root(graph)
  distances <- igraph::distances(
    graph,
    v = igraph::V(graph),
    mode = "out",
    to = root,
    weights = NA
  )
  distances <- data.frame(distances, tag_name = rownames(distances))
  colnames(distances) <- c("hierarchy_level", "tag_name")
  rownames(distances) <- NULL
  distances
}

get_initial_genres_tree_and_votes_based <- function(mb_tags, graph) {
  # determines the initial genre for each track by following
  # the tree branch with most votes as far as possible
  total_votes_genres <- mb_tags |>
    dplyr::group_by(tag_name) |>
    dplyr::summarize(votes_total = sum(tag_count))
  mb_tags <- mb_tags |> dplyr::inner_join(total_votes_genres, by = "tag_name")
  tracks <- unique(mb_tags$track.s.id)
  res <- c()
  for (track in tracks) {
    if (track %% 500 == 0) {
      message(sprintf("track %d of %d", track, length(tracks)))
    }
    track_tags <- mb_tags |> dplyr::filter(track.s.id == track)
    initial_genre <- get_tree_and_votes_based_mapping(track_tags, graph)
    res <- rbind(
      res,
      data.frame(track.s.id = track, initial_genre = initial_genre)
    )
  }
  res
}

get_tree_and_votes_based_mapping <- function(track_tags, graph) {
  if (nrow(track_tags) == 1) {
    return(track_tags$tag_name)
  }
  while (nrow(track_tags) > 1) {
    most_voted <- track_tags |>
      dplyr::arrange(-tag_count, votes_total) |>
      dplyr::first() |>
      dplyr::pull(tag_name)
    children <- get_subgraph(graph, most_voted) |>
      igraph::V() |>
      names()
    children <- children[children != most_voted]
    track_tags <- track_tags |> dplyr::filter(tag_name %in% children)
    if (nrow(track_tags) == 0) {
      genre <- most_voted
    }
    if (nrow(track_tags) == 1) {
      genre <- track_tags |> dplyr::pull(tag_name)
    }
  }
  genre
}
