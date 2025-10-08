# FUNCTIONS ----
filter_valid_mb_genres <- function(input, non_music_tags) {
  combined_tags <- get_combined_detailed_tags(input)
  filtered_non_empty_tags <- filter_non_empty_tags(combined_tags)
  filtered_valid_tags <- filter_music_tags(
    filtered_non_empty_tags,
    non_music_tags
  )
  filtered_valid_tags
}


get_combined_detailed_tags <- function(input) {
  input |>
    dplyr::mutate(
      mb.genres = purrr::pmap(
        list(
          .data$track.mb.genres,
          .data$album.mb.genres,
          .data$artist.mb.genres
        ),
        choose_mb_tag_set,
        .progress = "Combining tag sets in the data ..."
      )
    )
}

choose_mb_tag_set <- function(
  track_tags,
  album_tags,
  artist_tags
) {
  n_track_tags <- track_tags |> nrow()
  n_album_tags <- album_tags |> nrow()
  if (is.null(n_track_tags)) {
    n_track_tags <- 0
  }
  if (is.null(n_album_tags)) {
    n_album_tags <- 0
  }

  if (n_track_tags != 0) {
    return(track_tags)
  }
  if (n_album_tags != 0) {
    return(album_tags)
  }
  artist_tags
}

filter_non_empty_tags <- function(input) {
  input$is.nonempty <- lapply(input$mb.genres, function(x) nrow(x) > 0) |>
    as.logical() |>
    tidyr::replace_na(FALSE)
  input$is.tag_name <- lapply(input$mb.genres, function(x) {
    "tag_name" %in% colnames(x)
  }) |>
    as.logical()
  dplyr::filter(input, .data$is.nonempty & .data$is.tag_name) |>
    dplyr::select(-"is.nonempty", -"is.tag_name")
}

filter_music_tags <- function(input, non_music_tags) {
  # takes a while
  input$mb.genres <- erase_non_music_tags(input$mb.genres, non_music_tags)
  input$is.nonempty <- lapply(input$mb.genres, function(x) nrow(x) > 0) |>
    as.logical()
  dplyr::filter(input, .data$is.nonempty) |>
    dplyr::select(-"is.nonempty")
}

erase_non_music_tags <- function(mb_tags, non_music_tags) {
  mb_tags_valid <- purrr::map(
    mb_tags,
    function(frame) {
      frame |> dplyr::filter(!.data$tag_name %in% non_music_tags)
    },
    .progress = "Filtering out non-music tags ..."
  )
  mb_tags_valid
}

unpack_mb_genre_tags <- function(input) {
  input_select <- input |>
    dplyr::select(
      "track.s.id",
      "track.s.title",
      "track.s.firstartist.name",
      "mb.genres"
    )
  unpacked_mb_tags <- unpack_genre_tags(input_select$mb.genres)
  input_select |>
    dplyr::mutate(join_id = dplyr::row_number()) |>
    dplyr::inner_join(unpacked_mb_tags) |>
    dplyr::select(
      "track.s.id",
      "track.s.title",
      "track.s.firstartist.name",
      "mb.genres",
      "tag_name",
      "tag_count"
    )
}

unpack_genre_tags <- function(tags) {
  purrr::map_df(
    seq_along(tags),
    function(i) {
      df <- tags[[i]]
      df$join_id <- i
      df
    },
    .progress = "Unpacking genre tags ..."
  )
}
