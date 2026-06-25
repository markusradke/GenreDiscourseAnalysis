split_artist_level_popularity_release_year <- function(
  mb,
  test_size = 0.8,
  seed = 42
) {
  #' Split a long data frame of music tracks into train and test sets at the artist level, stratified by popularity and release year.
  #' @param mb A long data frame of music tracks with columns for artist ID, popularity, and release year.
  #' @param test_size The proportion of the data to include in the test set (default is 0.8).
  #' @param seed A random seed for reproducibility (default is 42).
  #' @return A list containing two elements: train_tracks and test_tracks, which are vectors of track IDs for the training and testing sets, respectively.
  artist_strata <- get_artist_strata(mb)
  split <- get_artist_level_strata_split(artist_strata, mb, test_size, seed)
  split_tracks <- extract_split_tracks(mb, split$train, split$test)
  split_tracks
}


get_artist_strata <- function(mb) {
  artist_strata <- mb |>
    group_by(track.s.firstartist.id) |>
    summarize(
      median_release_year = median(album.s.releaseyear, na.rm = TRUE),
      artist_popularity = median(artist.s.popularity, na.rm = TRUE) # should be same for all tracks of an artist
    ) |>
    mutate(
      popularity_bin = cut(
        artist_popularity,
        breaks = quantile(
          artist_popularity,
          probs = c(0, 0.33, 0.66, 1),
          na.rm = TRUE
        ),
        labels = FALSE
      ),
      release_year_bin = cut(
        median_release_year,
        quantile(
          median_release_year,
          probs = c(0, 0.33, 0.66, 1),
          na.rm = TRUE
        ),
        labels = FALSE
      ),
      strata = paste(popularity_bin, release_year_bin, sep = "_")
    )
  return(artist_strata)
}

get_artist_level_strata_split <- function(artist_strata, mb, test_size, seed) {
  set.seed(seed)
  split <- rsample::initial_split(
    artist_strata,
    prop = test_size,
    strata = "strata"
  )
  train <- rsample::training(split) |> select(track.s.firstartist.id)
  test <- rsample::testing(split) |> select(track.s.firstartist.id, strata)
  return(list(train = train, test = test))
}

extract_split_tracks <- function(mb, train, test) {
  train_tracks <- mb |>
    filter(track.s.firstartist.id %in% train$track.s.firstartist.id) |>
    pull(track.s.id)
  test_tracks <- mb |>
    filter(track.s.firstartist.id %in% test$track.s.firstartist.id) |>
    pull(track.s.id)
  return(list(train_tracks = train_tracks, test_tracks = test_tracks))
}


get_common_tags <- function(long, fold1_ids, fold2_ids, min_artists = 10) {
  #' Get common tags between two folds of a long data frame of music tracks, filtered by a minimum number of unique artists in both folds.
  #' @param long A long data frame of music tracks with columns for track ID, artist ID, and tag name.
  #' @param fold1_ids A vector of track IDs for the first fold.
  #' @param fold2_ids A vector of track IDs for the second fold.
  #' @param min_artists The minimum number of unique artists required for a tag to be considered common (default is 10).
  #' @return A vector of common tag names that meet the minimum artist requirement.
  fold1_artist_thresh <- long |>
    filter(track.s.id %in% fold1_ids) |>
    filter_tags_by_artist_occurrences(n_min_artists = min_artists)
  fold2_artist_thresh <- long |>
    filter(track.s.id %in% fold2_ids) |>
    filter_tags_by_artist_occurrences(n_min_artists = min_artists)

  common_tags <- intersect(
    unique(fold1_artist_thresh$tag_name),
    unique(fold2_artist_thresh$tag_name)
  )
  return(common_tags)
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
