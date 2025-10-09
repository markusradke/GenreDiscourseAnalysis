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
  message("Done.")
  filtered
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

get_combined_dc_tags <- function(input) {
  combine_tag_columns(
    input = input,
    cols = c("album.dc.genres", "album.dc.styles"),
    builder = dc_builder,
    outcol = "dc.genres",
    msg = "Combining DC genre and style tags ..."
  )
}

combine_tag_columns <- function(input, cols, builder, outcol, msg = "") {
  message(msg)
  n <- nrow(input)
  out_list <- vector("list", length = ifelse(is.null(n), 0, n))

  if (is.null(n) || n == 0) {
    input[[outcol]] <- out_list
    return(input)
  }

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)
  out_list <- lapply(seq_len(n), function(i) {
    utils::setTxtProgressBar(pb, i)
    args <- lapply(cols, function(col) input[[col]][[i]])
    do.call(builder, args)
  })
  input[[outcol]] <- out_list
  input
}

get_combined_s_tags <- function(input) {
  empty_tags_df <- function() {
    data.frame(
      tag_name = character(0),
      tag_count = integer(0),
      stringsAsFactors = FALSE
    )
  }

  process_one_sp <- function(genres) {
    # genres may be: NULL, empty data.frame, data.frame with column "genre" or "tag_name",
    if (is.null(genres)) {
      return(empty_tags_df())
    }

    if (is.data.frame(genres)) {
      if (nrow(genres) == 0) {
        return(empty_tags_df())
      }
      if ("genre" %in% names(genres)) {
        names(genres)[names(genres) == "genre"] <- "tag_name"
      }
      tags <- as.character(genres$tag_name)
      tags <- unique(tags)
      data.frame(
        tag_name = tags,
        tag_count = rep(1, length(tags)),
        stringsAsFactors = FALSE
      )
    } else {
      empty_tags_df()
    }
  }

  message("Combining Spotify artist genres ...")
  n <- nrow(input)
  s_list <- vector("list", length = ifelse(is.null(n), 0, n))

  if (is.null(n) || n == 0) {
    input$s.genres <- s_list
    return(input)
  }

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)
  s_list <- mapply(
    function(genres, idx) {
      utils::setTxtProgressBar(pb, idx)
      process_one_sp(genres)
    },
    input$artist.s.genres,
    seq_len(n),
    SIMPLIFY = FALSE
  )
  input$s.genres <- s_list
  input
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

dc_builder <- function(genres, styles) {
  tags <- unique(c(normalize_tags(genres), normalize_tags(styles)))
  data.frame(
    tag_name = tags,
    tag_count = rep(1, length(tags)),
    stringsAsFactors = FALSE
  )
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
  message("Erasing non-music tags ...")
  n <- length(tags)
  if (n == 0) {
    return(vector("list", 0))
  }
  if (length(non_music_tags) == 0) {
    return(tags)
  }

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)

  out <- lapply(seq_len(n), function(i) {
    utils::setTxtProgressBar(pb, i)
    frame <- tags[[i]]
    if (is.data.frame(frame) && "tag_name" %in% colnames(frame)) {
      keep <- is.na(match(frame$tag_name, non_music_tags))
      frame[keep, , drop = FALSE]
    } else {
      frame
    }
  })
  out
}

get_long_genre_tags <- function(input, genrecol) {
  unpacked_tags <- unpack_genre_tags(input[[genrecol]])
  join_mapping <- data.frame(
    join_id = seq_len(nrow(input)),
    track.s.id = input$track.s.id,
    track.s.title = input$track.s.title,
    track.s.firstartist.name = input$track.s.firstartist.name,
    stringsAsFactors = FALSE
  )

  message("Merging genre tags with track info ...")
  result <- merge(
    join_mapping,
    unpacked_tags,
    by = "join_id",
    sort = FALSE
  )[, c(
    "track.s.id",
    "track.s.title",
    "track.s.firstartist.name",
    "tag_name",
    "tag_count"
  )]
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
