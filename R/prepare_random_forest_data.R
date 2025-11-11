#' Prepare data for the random-forest training (including optional imputation)
#'
#' Runs the full data preparation pipeline up to and including optional
#' missForest imputation. Always returns train and test sets for both
#' low and high detail MusicBrainz metagenre sets ("low", "high").
#' Additionally creates cross-validation folds based on artist splits.
#'
#' @param settings A named list of pipeline settings (must contain
#'   \code{subsample_prop}, \code{casewise_threshold}, \code{seed},
#'   \code{artist_initial_split}, \code{apply_imputation}, \code{n_cores},
#'   \code{drop_POPULARMUSIC}, \code{cv_folds}, \code{cv_repeats}, \code{max_tracks_per_artist_cv}).
#' @param poptrag A data.frame or tibble containing the full poptrag dataset used for feature selection.
#' @return A list with elements:
#'   \describe{
#'     \item{imputer_model}{trained imputer or NULL}
#'     \item{datasets}{list with elements \code{low} and \code{high}, each a list with \code{train}, \code{test} data.frames, and \code{cv_splits}}
#'   }
#' @export
prepare_rf_data <- function(settings, poptrag) {
  check_settings <- setdiff(
    names(settings),
    c(
      "subsample_prop",
      "casewise_threshold",
      "seed",
      "artist_initial_split",
      "apply_imputation",
      "n_cores",
      "drop_POPULARMUSIC",
      "cv_folds",
      "cv_repeats",
      "max_tracks_per_artist_cv"
    )
  )
  if (!length(check_settings) == 0) {
    stop(sprintf(
      "Please provide a complete settings object. Missing settings: %s",
      paste(check_settings, collapse = ", ")
    ))
  }

  selected <- select_features_poptrag(poptrag)
  transformed <- transform_features(selected)
  casewise <- apply_casewise_filter(
    transformed,
    threshold = settings$casewise_threshold
  )

  low_met <- read_feather_with_lists(
    paste0("models/metagenres/mb_metagenres_10_15.feather")
  )
  high_met <- read_feather_with_lists(
    paste0("models/metagenres/mb_metagenres_25_30.feather")
  )

  low_joined <- join_target(casewise, low_met, settings$drop_POPULARMUSIC)
  high_joined <- join_target(casewise, high_met, settings$drop_POPULARMUSIC)

  if (settings$subsample_prop <= 0 || settings$subsample_prop > 1) {
    stop("settings$subsample_prop must be in (0, 1].")
  }
  if (settings$subsample_prop == 1) {
    low_sample <- low_joined
    high_sample <- high_joined
  } else {
    set.seed(settings$seed)
    low_sample <- draw_prototype_sample(
      low_joined,
      prop = settings$subsample_prop
    )
    high_sample <- draw_prototype_sample(
      high_joined,
      prop = settings$subsample_prop
    )
  }

  artisthigh_split <- split_artists_and_train_test(
    low_sample,
    high_sample,
    prop = settings$artist_initial_split,
    seed = settings$seed
  )

  low_imputer <- NULL
  high_imputer <- NULL
  if (settings$apply_imputation) {
    low_imputer <- train_imputer(
      artisthigh_split$low_train,
      nthreads = settings$n_cores,
      seed = settings$seed
    )
    low_train_imputed <- low_imputer$ximp
    low_test_imputed <- apply_imputer(artisthigh_split$low_test, low_imputer)

    high_imputer <- train_imputer(
      artisthigh_split$high_train,
      nthreads = settings$n_cores,
      seed = settings$seed
    )
    high_train_imputed <- high_imputer$ximp
    high_test_imputed <- apply_imputer(
      artisthigh_split$high_test,
      high_imputer
    )
  } else {
    low_train_imputed <- artisthigh_split$low_train
    low_test_imputed <- artisthigh_split$low_test
    high_train_imputed <- artisthigh_split$high_train
    high_test_imputed <- artisthigh_split$high_test
  }

  # Create CV folds with artist-based splitting
  message("---CREATING CV FOLDS---")
  low_cv_splits <- NULL
  high_cv_splits <- NULL

  if (!is.null(settings$cv_folds) && settings$cv_folds > 1) {
    low_cv_splits <- create_artist_cv_splits(
      low_train_imputed,
      n_folds = settings$cv_folds,
      repeats = settings$cv_repeats,
      max_tracks_per_artist = settings$max_tracks_per_artist_cv,
      seed = settings$seed
    )

    high_cv_splits <- create_artist_cv_splits(
      high_train_imputed,
      n_folds = settings$cv_folds,
      repeats = settings$cv_repeats,
      max_tracks_per_artist = settings$max_tracks_per_artist_cv,
      seed = settings$seed
    )

    # Sanity checks for factor levels
    check_factor_levels_in_folds(low_cv_splits, "low")
    check_factor_levels_in_folds(high_cv_splits, "high")
  }

  datasets <- list(
    low = list(
      train = low_train_imputed,
      test = low_test_imputed,
      imputer = low_imputer,
      cv_splits = low_cv_splits
    ),
    high = list(
      train = high_train_imputed,
      test = high_test_imputed,
      imputer = high_imputer,
      cv_splits = high_cv_splits
    )
  )

  datasets
}

select_features_poptrag <- function(poptrag) {
  poptrag |>
    dplyr::select(
      -dplyr::contains("track.es"),
      -dplyr::contains("album.dc")
    ) |>
    dplyr::select(
      track.s.id,
      artist.s.id,
      artist.s.popularity,
      artist.s.followers,
      album.s.totaltracks,
      album.s.releaseyear,
      album.s.popularity,
      track.s.danceability,
      track.s.energy,
      track.s.key,
      track.s.loudness,
      track.s.mode,
      track.s.speechiness,
      track.s.acousticness,
      track.s.instrumentalness,
      track.s.liveness,
      track.s.valence,
      track.s.tempo,
      track.s.timesignature,
      track.s.explicitlyrics,
      track.s.popularity,
      track.s.duration,
      album.mb.language,
      artist.mb.type,
      artist.mb.gender,
      artist.mb.area,
      artist.mb.birthyear,
      artist.mb.dead,
      artist.mb.origin,
      track.ab.p.danceable,
      track.ab.p.female,
      track.ab.p.acoustic,
      track.ab.p.aggressive,
      track.ab.p.electronic,
      track.ab.p.happy,
      track.ab.p.party,
      track.ab.p.relaxed,
      track.ab.p.sad,
      track.ab.p.bright,
      track.ab.p.tonal,
      track.ab.p.voice,
      track.ab.rhythm.tempo,
      track.ab.rhythm.danceability,
      track.ab.rhythm.onsetrate,
      track.ab.low.loudness,
      track.ab.low.dynamiccomplexity,
      track.ab.tonal.chordchangerate,
      track.ab.tonal.key,
      track.ab.tonal.chordsnumberrate,
      track.ab.tonal.mode,
      track.ab.tonal.keystrength,
      track.dz.rank,
      track.dz.tempo,
      track.dz.loudness,
      track.dz.firstartist.followers,
      track.dz.firstartist.nalbums,
      track.dz.album.explicitlyrics,
      track.dz.album.duration,
      track.dz.album.followers,
      track.language,
      track.is.instrumental,
      dplyr::contains("lyrics."),
      track.is.dach,
      dplyr::contains("label.med")
    )
}

transform_features <- function(df) {
  df |>
    dplyr::mutate(
      track.s.key = as.factor(track.s.key),
      track.s.mode = as.factor(track.s.mode),
      track.s.timesignature = as.factor(track.s.timesignature),
      track.s.explicitlyrics = as.factor(track.s.explicitlyrics),
      album.mb.language = as.factor(album.mb.language),
      artist.mb.type = as.factor(artist.mb.type),
      artist.mb.gender = as.factor(artist.mb.gender),
      artist.mb.area = as.factor(artist.mb.area),
      artist.mb.dead = as.factor(artist.mb.dead),
      artist.mb.origin = as.factor(artist.mb.origin),
      track.ab.tonal.key = as.factor(track.ab.tonal.key),
      track.ab.tonal.mode = as.factor(track.ab.tonal.mode),
      track.dz.album.explicitlyrics = as.factor(track.dz.album.explicitlyrics),
      track.language = as.factor(track.language),
      track.is.dach = as.factor(track.is.dach),
      lyrics.distinct_words_ratio = ifelse(
        track.is.instrumental,
        1,
        lyrics.distinct_words_ratio
      ),
      lyrics.repeated_lines_ratio = ifelse(
        track.is.instrumental,
        1,
        lyrics.repeated_lines_ratio
      ),
      lyrics.sentiment = ifelse(track.is.instrumental, 0, lyrics.sentiment)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("lyrics.nrc_"),
        ~ ifelse(track.is.instrumental, 0, .x)
      ),
      track.is.instrumental = as.factor(track.is.instrumental)
    ) |>
    dplyr::select(
      -artist.mb.origin,
    )
}

apply_casewise_filter <- function(df, threshold = 0.4) {
  df$n_NA <- rowSums(is.na(df))
  keep <- df |>
    dplyr::filter(.data$n_NA <= (ncol(df) - 2) * threshold)
  keep
}

join_target <- function(casewise, metagenres, drop_POPULARMUSIC) {
  joined <- casewise |>
    dplyr::inner_join(
      metagenres |>
        dplyr::select(track.s.id, metagenre),
      by = "track.s.id"
    ) |>
    dplyr::mutate(metagenre = as.factor(metagenre))
  if ("POPULAR MUSIC" %in% levels(joined$metagenre)) {
    if (drop_POPULARMUSIC) {
      joined <- joined |>
        dplyr::filter(metagenre != "POPULAR MUSIC") |>
        dplyr::mutate(metagenre = droplevels(metagenre))
    }
  }
  joined
}

draw_prototype_sample <- function(df, prop = 0.12) {
  rsample::initial_split(df, prop = prop, strata = metagenre) |>
    rsample::training()
}

# Produce train/test splits based on first artist genre.
split_artists_and_train_test <- function(
  low_sample,
  high_sample,
  prop = 0.5,
  seed = 42
) {
  low_dist <- low_sample |>
    dplyr::distinct(artist.s.id, .keep_all = TRUE)
  high_dist <- high_sample |>
    dplyr::distinct(artist.s.id, .keep_all = TRUE)

  set.seed(seed)
  low_split <- rsample::initial_split(low_dist, prop = prop, strata = metagenre)
  high_split <- rsample::initial_split(
    high_dist,
    prop = prop,
    strata = metagenre
  )

  low_train_art <- rsample::training(low_split)
  low_test_art <- rsample::testing(low_split)
  high_train_art <- rsample::training(high_split)
  high_test_art <- rsample::testing(high_split)

  low_train <- low_sample |>
    dplyr::inner_join(
      low_train_art |> dplyr::select(artist.s.id),
      by = "artist.s.id"
    )
  low_test <- low_sample |>
    dplyr::inner_join(
      low_test_art |> dplyr::select(artist.s.id),
      by = "artist.s.id"
    )
  high_train <- high_sample |>
    dplyr::inner_join(
      high_train_art |> dplyr::select(artist.s.id),
      by = "artist.s.id"
    )
  high_test <- high_sample |>
    dplyr::inner_join(
      high_test_art |> dplyr::select(artist.s.id),
      by = "artist.s.id"
    )

  list(
    low_train = low_train,
    low_test = low_test,
    high_train = high_train,
    high_test = high_test
  )
}

train_imputer <- function(impute_frame, nthreads = 19, seed = 42) {
  set.seed(seed)
  message("---TRAINING MISSFOREST IMPUTER---")
  missForestPredict::missForest(
    as.data.frame(
      impute_frame |>
        dplyr::select(-track.s.id, -metagenre, -n_NA)
    ),
    maxiter = 1,
    mtry = floor(sqrt(ncol(impute_frame) - 3)),
    replace = TRUE,
    verbose = TRUE,
    num.threads = nthreads
  )
}

apply_imputer <- function(df, imputer_model) {
  imputed_data <- missForestPredict::missForestPredict(
    imputer_model,
    as.data.frame(
      df |>
        dplyr::select(-track.s.id, -metagenre, -n_NA)
    )
  )
  df |>
    dplyr::select(track.s.id, metagenre) |>
    dplyr::bind_cols(as.data.frame(imputed_data))
}


#' Create artist-based cross-validation splits
#'
#' Creates CV folds ensuring no artist appears in multiple folds.
#' Artists with many tracks are undersampled to facilitate splitting.
#'
#' @param train_data Training data with artist.s.id and metagenre columns
#' @param n_folds Number of CV folds
#' @param repeats Number of repeated CV runs
#' @param max_tracks_per_artist Maximum tracks per artist before undersampling
#' @param seed Random seed
#' @return rsample vfold_cv object
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
  cv_splits <- lapply(seq_len(nrow(artist_folds)), function(fold_idx) {
    analysis_artists <- rsample::analysis(artist_folds$splits[[
      fold_idx
    ]])$artist.s.id
    assessment_artists <- rsample::assessment(artist_folds$splits[[
      fold_idx
    ]])$artist.s.id

    # Get corresponding tracks
    analysis_data <- train_undersampled |>
      dplyr::filter(artist.s.id %in% analysis_artists)
    assessment_data <- train_undersampled |>
      dplyr::filter(artist.s.id %in% assessment_artists)

    # Create a manual rsplit object
    list(
      analysis = analysis_data,
      assessment = assessment_data,
      fold = artist_folds$id[[fold_idx]],
      id2 = artist_folds$id2[[fold_idx]]
    )
  })

  cv_splits
}

#' Check factor levels in CV folds
#'
#' Prints warnings if any factor level has fewer than 30 observations
#' in any fold.
#'
#' @param cv_splits List of CV splits
#' @param dataset_name Name of dataset for messages
check_factor_levels_in_folds <- function(cv_splits, dataset_name) {
  if (is.null(cv_splits)) {
    return(invisible(NULL))
  }

  message(sprintf("Checking factor levels for %s dataset...", dataset_name))

  for (fold_idx in seq_along(cv_splits)) {
    fold_data <- cv_splits[[fold_idx]]$analysis

    # Get all factor columns (excluding metagenre which is the target)
    factor_cols <- names(fold_data)[
      vapply(fold_data, is.factor, logical(1))
    ]
    factor_cols <- setdiff(factor_cols, "metagenre")

    for (col in factor_cols) {
      level_counts <- table(fold_data[[col]])
      min_count <- min(level_counts)

      if (min_count < 30) {
        min_level <- names(level_counts)[which.min(level_counts)]
        warning(
          sprintf(
            "Fold %d (%s): Feature '%s', level '%s' has only %d observations",
            fold_idx,
            dataset_name,
            col,
            min_level,
            min_count
          ),
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}
