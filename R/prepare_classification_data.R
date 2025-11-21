#' Prepare data for the random-forest training (including optional imputation)
#'
#' Runs the full data preparation pipeline up to and including optional
#' missForest imputation. Always returns train and test sets for both
#' low and high detail MusicBrainz metagenre sets ("low", "high").
#' Additionally creates cross-validation folds based on artist splits.
#'
#' @param settings A named list of pipeline settings (must contain
#'   \code{subsample_prop}, \code{casewise_threshold}, \code{seed},
#'   \code{artist_initial_split}, \code{drop_POPULARMUSIC}, \code{cv_folds},
#'  \code{cv_repeats}, \code{max_tracks_per_artist_cv},
#'   \code{s_genremapping}, \code{min_level_total_set}, \code{maxiter_imp}, \code{min_n_factor_level}).
#' @param poptrag A data.frame or tibble containing the full poptrag dataset used for feature selection.
#' @return A list with elements:
#'   \describe{
#'     \item{imputer_model}{trained imputer or NULL}
#'     \item{datasets}{list with elements \code{low} and \code{high}, each a list with \code{train}, \code{test} data.frames, and \code{cv_splits}}
#'   }
#' @export
prepare_rf_data <- function(settings, poptrag, metagenres) {
  check_settings <- setdiff(
    names(settings),
    c(
      "subsample_prop",
      "casewise_threshold",
      "seed",
      "artist_initial_split",
      "drop_POPULARMUSIC",
      "cv_folds",
      "cv_repeats",
      "max_tracks_per_artist_cv",
      "s_genremapping",
      "min_n_factor_level"
    )
  )
  if (!length(check_settings) == 0) {
    stop(sprintf(
      "Please provide a complete settings object. Differing settings: %s",
      paste(check_settings, collapse = ", ")
    ))
  }

  selected <- select_features_poptrag(poptrag)
  transformed <- transform_features(
    selected,
    s_genremapping = settings$s_genremapping
  )
  casewise <- apply_casewise_filter(
    transformed,
    threshold = settings$casewise_threshold
  )

  joined <- join_target(casewise, metagenres, settings$drop_POPULARMUSIC)

  if (settings$subsample_prop <= 0 || settings$subsample_prop > 1) {
    stop("settings$subsample_prop must be in (0, 1].")
  }
  if (settings$subsample_prop == 1) {
    sampled <- joined
  } else {
    set.seed(settings$seed)
    sampled <- draw_prototype_sample(
      joined,
      prop = settings$subsample_prop
    )
  }

  artist_split <- split_artists_and_train_test(
    sampled,
    prop = settings$artist_initial_split,
    seed = settings$seed
  )
  train <- artist_split$train
  test <- artist_split$test

  message("---CONVERTING P_MAX TO CASE WEIGHTS---")
  train$case_wts <- hardhat::importance_weights(
    train$p_max
  )
  test$case_wts <- hardhat::importance_weights(
    test$p_max
  )
  train <- train |> dplyr::select(-p_max)
  test <- test |> dplyr::select(-p_max)

  message("---CREATING ARTIST-BASED CV FOLDS---")
  cv_splits <- create_artist_cv_splits(
    train,
    n_folds = settings$cv_folds,
    repeats = settings$cv_repeats,
    max_tracks_per_artist = settings$max_tracks_per_artist_cv,
    seed = settings$seed
  )
  cleaned <- clean_factor_levels_in_folds(
    cv_splits,
    train,
    test,
    settings$min_n_factor_level
  )
  cv_splits <- cleaned$cv_splits
  train <- cleaned$train_data
  test <- cleaned$test_data

  list(
    train = train,
    test = test,
    cv_splits = cv_splits
  )
}

select_features_poptrag <- function(poptrag) {
  poptrag |>
    dplyr::select(
      -dplyr::contains("track.es"),
      -dplyr::contains("album.dc")
    ) |>
    dplyr::select(
      dplyr::all_of(classification_prep_features)
    )
}

transform_features <- function(
  df,
  s_genremapping
) {
  origin_lookup <- artist_origin_lookup$country
  names(origin_lookup) <- artist_origin_lookup$original

  df |>
    dplyr::mutate(
      track.s.key = as.factor(track.s.key),
      track.s.mode = as.factor(track.s.mode),
      track.s.explicitlyrics = as.factor(track.s.explicitlyrics),
      artist.mb.area = as.factor(artist.mb.area),
      artist.mb.origin = origin_lookup[artist.mb.origin] |> setNames(NULL),
      artist.mb.origin = as.factor(artist.mb.origin),
      artist.mb.dead = as.factor(artist.mb.dead),
      artist.mb.origin = as.factor(artist.mb.origin),
      track.ab.tonal.key = as.factor(track.ab.tonal.key),
      track.ab.tonal.mode = as.factor(track.ab.tonal.mode),
      track.dz.album.explicitlyrics = as.factor(track.dz.album.explicitlyrics),
      track.language = as.factor(track.language),
      track.is.dach = as.factor(track.is.dach),
      is.major.label = as.factor(is.major.label),
      artist.mb.gender = as.factor(artist.mb.gender),
      artist.mb.type = ifelse(
        stringr::str_detect(
          artist.mb.type,
          "Group|Orchestra|Choir|Orchestra"
        ),
        "Group",
        artist.mb.type
      ),
      artist.mb.type = ifelse(
        stringr::str_detect(
          artist.mb.type,
          "Person|Character"
        ),
        "Person",
        artist.mb.type
      ),
      artist.mb.type = as.factor(artist.mb.type),
      track.s.timesignature = as.factor(track.s.timesignature),
      lyrics.distinct_words_ratio = ifelse(
        track.is.instrumental,
        0,
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
    unnest_distribution_genres(
      mapping = s_genremapping
    )
}

unnest_distribution_genres <- function(df, mapping) {
  message("---MAPPING AND CREATING DISTRIBUTION GENRE FLAGS---")

  unique_metagenres <- unique(mapping$metagenre)
  col_names <- paste0("dtb.", gsub("[ -]", ".", unique_metagenres)) |> tolower()

  genre_lookup <- setNames(mapping$metagenre, mapping$tag_name)

  genre_flags_list <- lapply(df$artist.s.genres, function(genres_df) {
    genres <- genres_df$genre
    mapped <- unique(genre_lookup[genres])
    mapped <- mapped[!is.na(mapped)]

    if (length(mapped) == 0) {
      mapped <- "POPULAR MUSIC"
    }

    flags <- setNames(rep(FALSE, length(unique_metagenres)), col_names)
    mapped_cols <- paste0("dtb.", gsub("[ -]", ".", mapped)) |> tolower()
    flags[mapped_cols] <- TRUE

    flags
  })

  genre_flags_df <- as.data.frame(do.call(rbind, genre_flags_list))

  if ("dtb.popular.music" %in% colnames(genre_flags_df)) {
    colnames(genre_flags_df)[
      colnames(genre_flags_df) == "dtb.popular.music"
    ] <- "dtb.other"
  }
  genre_flags_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.factor)) |>
    dplyr::bind_cols(df) |>
    dplyr::select(-artist.s.genres)
}

apply_casewise_filter <- function(df, threshold = 0.4) {
  df$n_NA <- rowSums(is.na(df))
  keep <- df |>
    dplyr::filter(.data$n_NA <= (ncol(df) - 2) * threshold) |>
    dplyr::select(-n_NA)
  keep
}

join_target <- function(casewise, metagenres, drop_POPULARMUSIC) {
  joined <- casewise |>
    dplyr::inner_join(
      metagenres |>
        dplyr::select(track.s.id, p_max, metagenre),
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
  sampled,
  prop = 0.5,
  seed = 42
) {
  dist <- sampled |>
    dplyr::distinct(artist.s.id, .keep_all = TRUE)

  set.seed(seed)
  splitted <- rsample::initial_split(dist, prop = prop, strata = metagenre)

  train_art <- rsample::training(splitted)
  test_art <- rsample::testing(splitted)

  train <- sampled |>
    dplyr::inner_join(
      train_art |> dplyr::select(artist.s.id),
      by = "artist.s.id"
    )
  test <- sampled |>
    dplyr::inner_join(
      test_art |> dplyr::select(artist.s.id),
      by = "artist.s.id"
    )

  # p_max is kept in the data and will be converted to case_wts later
  list(
    train = train,
    test = test
  )
}
