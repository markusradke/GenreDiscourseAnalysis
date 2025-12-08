#' Create artist-based cross-validation splits
#'
#' Creates CV folds ensuring no artist appears in multiple folds.
#' Artists with many tracks are undersampled to facilitate splitting.
#' Returns a tidymodels-compatible vfold_cv rset where each split's
#' $data is the undersampled track-level dataset used to map artists.
#'
#' @param train_data Data frame with artist.s.id and metagenre columns
#' @param n_folds Number of CV folds
#' @param repeats Number of repeated CV runs
#' @param max_tracks_per_artist Maximum tracks per artist before sampling
#' @param seed Random seed (set at top level for reproducibility)
#' @return rsample vfold_cv object with artist-level splits mapped to tracks
create_artist_cv_splits <- function(
  train_data,
  n_folds = 5,
  repeats = 1,
  max_tracks_per_artist = 50,
  seed = 42
) {
  set.seed(seed)
  train_us <- undersample_artists(train_data, max_tracks_per_artist)
  artist_df <- make_unique_artists(train_us)
  artist_folds <- make_artist_folds(artist_df, n_folds, repeats)
  splits_list <- map_artist_folds_to_track_splits(artist_folds, train_us)
  build_cv_rset(splits_list, artist_folds, n_folds, repeats)
}

undersample_artists <- function(train_data, max_tracks_per_artist) {
  groups <- split(train_data, train_data$artist.s.id)
  undersampled <- lapply(groups, function(tracks) {
    if (nrow(tracks) > max_tracks_per_artist) {
      tracks[sample(nrow(tracks), max_tracks_per_artist), , drop = FALSE]
    } else {
      tracks
    }
  })
  dplyr::bind_rows(undersampled)
}

make_unique_artists <- function(train_undersampled) {
  train_undersampled |>
    dplyr::distinct(artist.s.id, .keep_all = TRUE) |>
    dplyr::select(artist.s.id, metagenre)
}

make_artist_folds <- function(artist_data, n_folds, repeats) {
  rsample::vfold_cv(
    artist_data,
    v = n_folds,
    repeats = repeats,
    strata = metagenre
  )
}

map_artist_folds_to_track_splits <- function(artist_folds, train_undersampled) {
  nrows <- nrow(artist_folds)
  lapply(seq_len(nrows), function(i) {
    art_analysis <- rsample::analysis(artist_folds$splits[[i]])$artist.s.id
    art_assess <- rsample::assessment(artist_folds$splits[[i]])$artist.s.id
    analysis_idx <- which(train_undersampled$artist.s.id %in% art_analysis)
    assessment_idx <- which(train_undersampled$artist.s.id %in% art_assess)
    rsample::make_splits(
      x = list(analysis = analysis_idx, assessment = assessment_idx),
      data = train_undersampled
    )
  })
}

build_cv_rset <- function(splits_list, artist_folds, n_folds, repeats) {
  cv_rset <- tibble::tibble(
    splits = splits_list,
    id = artist_folds$id
  )
  if ("id2" %in% names(artist_folds)) {
    cv_rset$id2 <- artist_folds$id2
  }
  class(cv_rset) <- c("vfold_cv", "rset", "tbl_df", "tbl", "data.frame")
  attr(cv_rset, "v") <- n_folds
  attr(cv_rset, "repeats") <- repeats
  attr(cv_rset, "strata") <- TRUE
  cv_rset
}
