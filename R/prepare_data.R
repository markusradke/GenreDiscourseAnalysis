# FUNCTIONS ----
combine_mb_genres <- function(input) {
  combine_genres(input, get_combined_mb_tags, "mb.genres")
}

combine_s_genres <- function(
  input,
  spotify_artist_genres
) {
  combine_genres(
    input,
    get_combined_s_tags,
    "s.genres",
    spotify_artist_genres
  )
}

combine_genres <- function(
  input,
  combine_fun,
  genrecol,
  ...
) {
  combined <- combine_fun(input, ...)
  non_empty <- filter_non_empty_tags(combined, genrecol)
  message("Done.")
  non_empty
}

get_combined_mb_tags <- function(input) {
  combine_tag_columns(
    input = input,
    cols = c("track.mb.genres", "album.mb.genres", "artist.mb.genres"),
    builder = choose_mb_tag_set,
    outcol = "mb.genres",
    msg = "Combining MusicBrainz tracks ..."
  )
}

combine_tag_columns <- function(input, cols, builder, outcol, msg = "") {
  message(msg)
  n <- nrow(input)
  out_list <- vector("list", length = ifelse(is.null(n), 0, n))
  source_list <- vector("character", length = ifelse(is.null(n), 0, n))

  if (is.null(n) || n == 0) {
    input[[outcol]] <- out_list
    input[[paste0(outcol, ".source")]] <- source_list
    return(input)
  }

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)

  results <- lapply(seq_len(n), function(i) {
    utils::setTxtProgressBar(pb, i)
    args <- lapply(cols, function(col) input[[col]][[i]])
    do.call(builder, args)
  })

  out_list <- lapply(results, function(x) x$tags)
  source_list <- sapply(results, function(x) x$source)

  input[[outcol]] <- out_list
  input[[paste0(outcol, ".source")]] <- source_list
  input
}

get_combined_s_tags <- function(input, spotify_artist_genres) {
  message("Combining Spotify artist genres for tracks with multiple artists...")
  n <- nrow(input)
  out_list <- vector("list", length = ifelse(is.null(n), 0, n))
  if (is.null(n) || n == 0) {
    input$s.genres <- out_list
    return(input)
  }

  genre_map <- build_artist_genre_map(spotify_artist_genres)

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)

  out_list <- lapply(seq_len(n), function(i) {
    utils::setTxtProgressBar(pb, i)
    track_artists <- get_track_artists_safe(input, i)
    if (is.null(track_artists)) {
      return(make_empty_tag_df())
    }
    ids <- extract_artist_ids(track_artists)
    genres_for_ids <- lookup_genres_for_ids(ids, genre_map)
    combine_genre_counts(genres_for_ids, length(ids))
  })

  input$s.genres <- out_list
  input
}

build_artist_genre_map <- function(artist_genres) {
  if (is.null(artist_genres) || nrow(artist_genres) == 0) {
    return(list())
  }
  ids <- as.character(artist_genres$artist.s.id)
  genres <- artist_genres$artist.s.genres
  setNames(genres, ids)
}

make_empty_tag_df <- function() {
  data.frame(
    tag_name = character(0),
    tag_count = integer(0),
    stringsAsFactors = FALSE
  )
}


get_track_artists_safe <- function(input, i) {
  ta <- input$track.s.artists[[i]]
  if (is.null(ta) || nrow(ta) == 0) {
    return(NULL)
  }
  ta
}

extract_artist_ids <- function(track_artists) {
  as.character(track_artists$id)
}

lookup_genres_for_ids <- function(ids, genre_map) {
  lapply(ids, function(aid) {
    g <- genre_map[[aid]]$genre
    if (is.null(g)) character(0) else as.character(g)
  })
}

combine_genre_counts <- function(genre_lists, n_trackartists) {
  combined <- unlist(genre_lists, use.names = FALSE)
  if (length(combined) == 0) {
    return(make_empty_tag_df())
  }
  if (n_trackartists == 1L) {
    data.frame(
      tag_name = combined,
      tag_count = rep.int(1L, length(combined)),
      stringsAsFactors = FALSE
    )
  } else {
    tab <- as.data.frame(table(combined), stringsAsFactors = FALSE)
    names(tab) <- c("tag_name", "tag_count")
    tab$tag_count <- as.integer(tab$tag_count)
    tab
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
    return(list(tags = track_tags, source = "track"))
  }
  if (n_album_tags != 0) {
    return(list(tags = album_tags, source = "album"))
  }
  list(tags = artist_tags, source = "artist")
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

get_long_genre_tags <- function(
  input,
  genrecol,
  caluclate_tag_count = character()
) {
  unpacked_tags <- unpack_genre_tags(input[[genrecol]])

  source_col <- paste0(genrecol, ".source")
  has_source <- source_col %in% colnames(input)

  join_mapping <- data.frame(
    join_id = seq_len(nrow(input)),
    track.s.id = input$track.s.id,
    album.dc.id = input$album.dc.id,
    trackartists.s.id = input$trackartists.s.id,
    track.s.title = input$track.s.title,
    track.s.firstartist.id = input$track.s.firstartist.id,
    track.s.firstartist.name = input$track.s.firstartist.name,
    n_trackartists = input$n_trackartists,
    stringsAsFactors = FALSE
  )

  if (has_source) {
    join_mapping$tag_source <- input[[source_col]]
  }

  message("Merging genre tags with track info ...")
  result <- dplyr::inner_join(
    join_mapping,
    unpacked_tags,
    by = "join_id"
  )

  select_cols <- c(
    "track.s.id",
    "album.dc.id",
    "trackartists.s.id",
    "track.s.title",
    "track.s.firstartist.id",
    "track.s.firstartist.name",
    "n_trackartists",
    "tag_name",
    "tag_count"
  )

  if (has_source) {
    select_cols <- c(select_cols, "tag_source")
  }

  result <- result |> dplyr::select(dplyr::all_of(select_cols))

  if (length(caluclate_tag_count) > 0) {
    message("Calculating tag counts ...")
    if (
      !all(caluclate_tag_count %in% c("artist", "total", "ones")) ||
        length(caluclate_tag_count) > 1
    ) {
      stop("If caluclate_tag_count is used, it must be 'artist' or 'total'.")
    } else {
      result <- calculate_tag_counts(result, caluclate_tag_count, has_source)
    }
  }
  message("Done.")
  result
}

unpack_genre_tags <- function(tags) {
  tagged_frames <- vector("list", length(tags))
  message("Unpacking genre tags ...")
  pb <- utils::txtProgressBar(min = 0, max = length(tags), style = 3)
  on.exit(close(pb), add = TRUE)

  for (i in seq_along(tags)) {
    idx <- tags[i]
    df <- idx[[1]]
    df$join_id <- i
    tagged_frames[[i]] <- df
    utils::setTxtProgressBar(pb = pb, value = i)
  }
  message("\nBinding tags ...")
  do.call(rbind, tagged_frames)
}

calculate_tag_counts <- function(tags, method, preserve_source = FALSE) {
  source_col <- if (preserve_source && "tag_source" %in% colnames(tags)) {
    tags$tag_source
  } else {
    NULL
  }

  tags <- tags |> dplyr::select(-"tag_count")
  if (!is.null(source_col)) {
    tags <- tags |> dplyr::select(-"tag_source")
  }

  if (method == "artist") {
    counts <- tags |>
      dplyr::group_by(.data$tag_name, .data$track.s.firstartist.name) |>
      dplyr::summarize(tag_count = dplyr::n(), .groups = "drop")
    result <- dplyr::right_join(
      tags,
      counts,
      by = c("tag_name", "track.s.firstartist.name")
    )
  } else if (method == "total") {
    counts <- tags |>
      dplyr::group_by(.data$tag_name) |>
      dplyr::summarize(tag_count = dplyr::n(), .groups = "drop")
    result <- dplyr::right_join(
      tags,
      counts,
      by = c("tag_name")
    )
  } else if (method == "ones") {
    tags$tag_count <- 1
    result <- tags
  } else {
    stop("Unknown method for calculating tag counts.")
  }

  select_cols <- c(
    "track.s.id",
    "album.dc.id",
    "trackartists.s.id",
    "track.s.title",
    "track.s.firstartist.id",
    "track.s.firstartist.name",
    "n_trackartists",
    "tag_name",
    "tag_count"
  )

  if (!is.null(source_col)) {
    result$tag_source <- source_col
    select_cols <- c(select_cols, "tag_source")
  }

  result |> dplyr::select(dplyr::all_of(select_cols))
}


get_unique_mb_tags <- function(mb_input) {
  all_tags <- c(
    unlist_mb_tags_dataframe(mb_input$track.mb.genres),
    unlist_mb_tags_dataframe(mb_input$album.mb.genres),
    unlist_mb_tags_dataframe(mb_input$artist.mb.genres)
  )
  unique(all_tags)
}

unlist_mb_tags_dataframe <- function(mb_tags) {
  unlist(lapply(mb_tags, function(x) {
    if (is.data.frame(x) && "tag_name" %in% colnames(x)) {
      as.character(x$tag_name)
    } else {
      character(0)
    }
  }))
}

filter_non_valid_tags <- function(long, non_music_tags) {
  dplyr::filter(
    long,
    !(.data$tag_name %in% non_music_tags)
  )
}

filter_tags_by_artist_occurrences <- function(long, n_min_artists) {
  tag_artist_counts <- long |>
    dplyr::group_by(.data$tag_name) |>
    dplyr::summarize(
      n_artists = dplyr::n_distinct(.data$track.s.firstartist.id),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_artists >= n_min_artists)

  dplyr::inner_join(
    long,
    tag_artist_counts,
    by = "tag_name"
  )
}
