#' Create artist-based cross-validation splits
#'
#' Creates CV folds ensuring no artist appears in multiple folds.
#' Artists with many tracks are undersampled to facilitate splitting.
#' Returns tidymodels-compatible rset object.
#'
#' @param train_data Training data with artist.s.id and metagenre columns
#' @param n_folds Number of CV folds
#' @param repeats Number of repeated CV runs
#' @param max_tracks_per_artist Maximum tracks per artist before undersampling
#' @param seed Random seed
#' @return rsample vfold_cv object with artist-level splits
create_artist_cv_splits <- function(
  train_data,
  n_folds = 5,
  repeats = 1,
  max_tracks_per_artist = 50,
  seed = 42
) {
  # Undersample artists with too many tracks
  set.seed(seed)
  artist_groups <- split(train_data, train_data$artist.s.id)
  artist_undersampled <- lapply(artist_groups, function(artist_tracks) {
    if (nrow(artist_tracks) > max_tracks_per_artist) {
      artist_tracks[
        sample(nrow(artist_tracks), max_tracks_per_artist),
        ,
        drop = FALSE
      ]
    } else {
      artist_tracks
    }
  })
  train_undersampled <- dplyr::bind_rows(artist_undersampled)

  # Get unique artists with their metagenres (use first occurrence)
  artist_data <- train_undersampled |>
    dplyr::distinct(artist.s.id, .keep_all = TRUE) |>
    dplyr::select(artist.s.id, metagenre)

  # Create artist-level folds
  set.seed(seed)
  artist_folds <- rsample::vfold_cv(
    artist_data,
    v = n_folds,
    repeats = repeats,
    strata = metagenre
  )

  # Convert artist-level folds to track-level folds
  splits_list <- lapply(seq_len(nrow(artist_folds)), function(fold_idx) {
    analysis_artists <- rsample::analysis(artist_folds$splits[[
      fold_idx
    ]])$artist.s.id
    assessment_artists <- rsample::assessment(artist_folds$splits[[
      fold_idx
    ]])$artist.s.id

    # Get row indices for corresponding tracks
    analysis_idx <- which(train_undersampled$artist.s.id %in% analysis_artists)
    assessment_idx <- which(
      train_undersampled$artist.s.id %in% assessment_artists
    )

    # Create proper rsplit object using make_splits
    rsample::make_splits(
      x = list(analysis = analysis_idx, assessment = assessment_idx),
      data = train_undersampled
    )
  })

  # Create a proper rset tibble
  cv_rset <- tibble::tibble(
    splits = splits_list,
    id = artist_folds$id
  )

  # Add id2 column if repeats > 1
  if (repeats > 1) {
    cv_rset$id2 <- artist_folds$id2
  }

  # Add proper classes and attributes for tidymodels compatibility
  class(cv_rset) <- c("vfold_cv", "rset", "tbl_df", "tbl", "data.frame")
  attr(cv_rset, "v") <- n_folds
  attr(cv_rset, "repeats") <- repeats
  attr(cv_rset, "strata") <- TRUE

  cv_rset
}
