# FUNCTIONS ----
select_relevant_columns <- function(input) {
  input |>
    dplyr::select(
      track.s.id,
      track.s.title,
      track.s.artists,
      track.s.firstartist.id,
      track.s.firstartist.name,
      track.s.popularity,
      track.s.previewurl,
      track.ab.genrerosamerica,
      dplyr::contains("ab.p.rosa."),
      track.dz.album.firstgenre.name,
      artist.s.id,
      artist.s.name,
      artist.s.genres,
      artist.s.popularity,
      album.s.id,
      album.s.title,
      album.s.releaseyear,
      album.s.popularity,
      album.s.coverurl,
      album.dc.id,
      album.dc.genres,
      album.dc.styles,
      track.dz.album.genres,
      track.mb.genres,
      album.mb.genres,
      artist.mb.genres,
      track.mb.combinedgenre,
      source.officialcharts,
      source.recommendations,
      source.featuredplaylists,
      source.spotifycharts
    ) |>
    dplyr::mutate(
      trackartists.s.id = sapply(.data$track.s.artists, function(x) {
        paste(x$id, collapse = ";")
      }),
      n_trackartists = sapply(.data$track.s.artists, function(x) {
        as.integer(nrow(x))
      }),
      album.dc.genres_str = sapply(.data$album.dc.genres, function(x) {
        if (is.list(x)) {
          paste(unlist(x), collapse = "; ")
        } else {
          NA
        }
      }),
      album.dc.styles_str = sapply(.data$album.dc.styles, function(x) {
        if (is.list(x)) {
          paste(unlist(x), collapse = "; ")
        } else {
          NA
        }
      }),
      album.dz.genres_str = sapply(.data$track.dz.album.genres, function(x) {
        if (is.list(x) && length(x) > 0) {
          genres <- sapply(x, function(genre_list) {
            if (is.list(genre_list) && "name" %in% names(genre_list)) {
              as.character(genre_list$name)
            } else {
              NULL
            }
          })
          if (length(genres) == 1 && is.null(genres[[1]])) {
            NA
          } else {
            paste(unlist(genres), collapse = "; ")
          }
        } else {
          NA
        }
      }),
      # correct parsing error in spotilink
      album.dz.genres_str = ifelse(
        !is.na(.data$track.dz.album.firstgenre.name) &
          !is.na(.data$album.dz.genres_str),
        paste(
          .data$track.dz.album.firstgenre.name,
          .data$album.dz.genres_str,
          sep = "; "
        ),
        .data$album.dz.genres_str
      ),
      album.dz.genres_str = ifelse(
        !is.na(.data$track.dz.album.firstgenre.name) &
          is.na(.data$album.dz.genres_str),
        .data$track.dz.album.firstgenre.name,
        .data$album.dz.genres_str
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "track.s.artists",
        "album.dc.genres",
        "album.dc.styles",
        "track.dz.album.genres"
      ))
    )
}

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

  input$join_id <- seq_len(nrow(input))

  message("Merging genre tags with track info ...")
  result <- dplyr::inner_join(
    input,
    unpacked_tags,
    by = "join_id"
  )

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

filter_min_votes <- function(
  mb_long,
  n_min_votes
) {
  i <- 0
  while (TRUE) {
    message(
      sprintf(
        "Denoising MusicBrainz tags: iteration %d, votes >= %d",
        i,
        n_min_votes
      )
    )
    mb_long_denoise_tracks <- filter_tracks_by_min_votes(
      mb_long,
      min_votes = n_min_votes
    )
    if (nrow(mb_long_denoise_tracks) == nrow(mb_long)) {
      break
    }
    mb_long <- mb_long_denoise_tracks
    i <- i + 1
  }
  return(mb_long_denoise_tracks)
}

filter_tracks_by_min_votes <- function(mb_long, min_votes = 2) {
  mb_total_votes <- mb_long |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::summarize(
      n_votes = sum(.data$tag_count),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_votes >= min_votes) |>
    dplyr::select(track.s.id)

  mb_long_filtered <- mb_long |>
    dplyr::inner_join(mb_total_votes, by = "track.s.id")

  return(mb_long_filtered)
}
