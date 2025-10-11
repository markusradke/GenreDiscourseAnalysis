# todo write tests
# todo test if it works for larger datasets
# todo optimize: This takes very long for large datasets
get_initial_genre_mapping <- function(tags, graph) {
  tags_in_graph <- tags |>
    dplyr::filter(.data$tag_name %in% igraph::V(graph)$name)
  initial_genres <- get_initial_genres_tree_and_votes_based(
    tags_in_graph,
    graph
  )
  unrepresented_ids <- setdiff(
    unique(tags$track.s.id),
    initial_genres$track.s.id
  )
  unrepresented_tracks <- tags |>
    dplyr::filter(.data$track.s.id %in% unrepresented_ids) |>
    dplyr::select(-"tag_name", -"tag_count") |>
    dplyr::mutate(initial_genre = NA) |>
    dplyr::distinct()
  rbind(initial_genres, unrepresented_tracks) |>
    dplyr::arrange(.data$track.s.id)
}


get_initial_genres_tree_and_votes_based <- function(tags, graph) {
  # determines the initial genre for each track by following
  # the tree branch with most votes as far as possible
  total_votes_genres <- tags |>
    dplyr::group_by(.data$tag_name) |>
    dplyr::summarize(votes_total = sum(tag_count))
  tags <- tags |> dplyr::inner_join(total_votes_genres, by = "tag_name")
  tracks <- unique(tags$track.s.id)
  res <- c()
  for (i in seq_along(tracks)) {
    if (i %% 500 == 0) {
      message(sprintf("track %d of %d", track, length(tracks[i])))
    }
    track_tags <- tags |> dplyr::filter(track.s.id == tracks[i])
    initial_genre <- get_tree_and_votes_based_mapping(track_tags, graph)
    res <- rbind(
      res,
      data.frame(
        track.s.id = tracks[i],
        initial_genre = initial_genre,
        track.s.title = track_tags$track.s.title[1],
        track.s.firstartist.name = track_tags$track.s.firstartist.name[1]
      )
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
      dplyr::arrange(-.data$tag_count, .data$votes_total) |>
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
