# FUNCTIONS ----
filter_valid_mb_genres <- function(input, non_music_tags) {
  filter_valid_genres(input, non_music_tags, get_combined_mb_tags, "mb.genres")
}

filter_valid_dc_genres <- function(input, non_music_tags) {
  filter_valid_genres(input, non_music_tags, get_combined_dc_tags, "dc.genres")
}

filter_valid_s_genres <- function(input, non_music_tags) {
  filter_valid_genres(input, non_music_tags, get_combined_s_tags, "s.genres")
}

filter_valid_genres <- function(input, non_music_tags, combine_fun, genrecol) {
  combined <- combine_fun(input)
  non_empty <- filter_non_empty_tags(combined, genrecol)
  filtered <- filter_music_tags(non_empty, genrecol, non_music_tags)
  filtered
}

get_combined_mb_tags <- function(input) {
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


get_combined_dc_tags <- function(input) {
  input |>
    dplyr::mutate(
      dc.genres = purrr::map2(
        .data$album.dc.genres,
        .data$album.dc.styles,
        function(genres, styles) {
          tags <- unique(c(normalize_tags(genres), normalize_tags(styles)))
          data.frame(tag_name = tags, tag_count = rep(1, length(tags)))
        },
        .progress = "Combining DC genre and style tags ..."
      )
    )
}

get_combined_s_tags <- function(input) {
  input |>
    dplyr::mutate(
      s.genres = purrr::map(
        .data$artist.s.genres,
        function(genres) {
          genres |>
            dplyr::rename(tag_name = "genre") |>
            dplyr::mutate(tag_count = 1)
        },
        .progress = "Normalizing Spotify artist genre tags ..."
      )
    )
}

normalize_tags <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    character(0)
  } else {
    as.character(x)
  }
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

filter_non_empty_tags <- function(input, genrecol) {
  input$is.nonempty <- lapply(input[[genrecol]], function(x) nrow(x) > 0) |>
    as.logical() |>
    tidyr::replace_na(FALSE)
  input$is.tag_name <- lapply(input[[genrecol]], function(x) {
    "tag_name" %in% colnames(x)
  }) |>
    as.logical()
  dplyr::filter(input, .data$is.nonempty & .data$is.tag_name) |>
    dplyr::select(-"is.nonempty", -"is.tag_name")
}

filter_music_tags <- function(input, genrecol, non_music_tags) {
  # takes a while
  input[[genrecol]] <- erase_non_music_tags(input[[genrecol]], non_music_tags)
  input$is.nonempty <- lapply(input[[genrecol]], function(x) nrow(x) > 0) |>
    as.logical()
  dplyr::filter(input, .data$is.nonempty) |>
    dplyr::select(-"is.nonempty")
}

erase_non_music_tags <- function(tags, non_music_tags) {
  purrr::map(
    tags,
    function(frame) {
      frame |> dplyr::filter(!.data$tag_name %in% non_music_tags)
    },
    .progress = "Filtering out non-music tags ..."
  )
}

get_long_genre_tags <- function(input, genrecol) {
  input_select <- input |>
    dplyr::select(
      "track.s.id",
      "track.s.title",
      "track.s.firstartist.name",
      dplyr::all_of(genrecol),
    )
  unpacked_mb_tags <- unpack_genre_tags(input_select[[genrecol]])
  suppressMessages(
    input_select |>
      dplyr::mutate(join_id = dplyr::row_number()) |>
      dplyr::inner_join(unpacked_mb_tags) |>
      dplyr::select(
        "track.s.id",
        "track.s.title",
        "track.s.firstartist.name",
        dplyr::all_of(genrecol),
        "tag_name",
        "tag_count"
      )
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
