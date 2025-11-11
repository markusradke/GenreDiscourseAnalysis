#' Prepare data for the random-forest training (including optional imputation)
#'
#' Runs the full data preparation pipeline up to and including optional
#' missForest imputation. Always returns train and test sets for both
#' low and high detail MusicBrainz metagenre sets ("low", "high").
#'
#' @param settings A named list of pipeline settings (must contain
#'   \code{subsample_prop}, \code{casewise_threshold}, \code{seed},
#'   \code{artist_initial_split}, \code{apply_imputation}, \code{n_cores},
#'   \code{drop_POPULARMUSIC}.
#' @param poptrag A data.frame or tibble containing the full poptrag dataset used for feature selection.
#' @return A list with elements:
#'   \describe{
#'     \item{imputer_model}{trained imputer or NULL}
#'     \item{datasets}{list with elements \code{low} and \code{high}, each a list with \code{train} and \code{test} data.frames}
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
      "drop_POPULARMUSIC"
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

  datasets <- list(
    low = list(
      train = low_train_imputed,
      test = low_test_imputed,
      imputer = low_imputer
    ),
    high = list(
      train = high_train_imputed,
      test = high_test_imputed,
      imputer = high_imputer
    )
  )

  datasets
}

#' Train and evaluate random-forest models for MusicBrainz with low and high metagenre detail
#'
#' Selects the post-imputation features, optionally undersamples, trains
#' a ranger random-forest and evaluates/plots results for each set of metagenres.
#'
#' @param settings Named list of pipeline settings (must contain \code{features_after_impute},
#'   \code{undersample_factor}, \code{varimp_top_n}, \code{seed}, \code{run_rf_mb}, \code{run_rf_s}).
#' @param datasets A list as returned by prepare_rf_data() with elements \code{low} and \code{high},
#'   each containing \code{train} and \code{test} data.frames.
#' @return A list with elements \code{low} and \code{high} (each either NULL or a list
#'   with \code{model}, \code{evaluation}, \code{train_df}, \code{test_df}).
#' @export
train_and_evaluate_rf <- function(settings, datasets) {
  if (
    is.null(settings$features_after_impute) ||
      !is.character(settings$features_after_impute)
  ) {
    stop("Please provide settings$features_after_impute as character vector.")
  }
  feats <- settings$features_after_impute
  if (!"metagenre" %in% feats) {
    feats <- c("metagenre", feats)
  }

  results <- list()

  for (detail in c("low", "high")) {
    run_flag <- isTRUE(settings[[paste0("run_rf_", detail)]])
    if (!run_flag) {
      results[[detail]] <- NULL
      next
    }

    train_df <- select_post_impute_features(datasets[[detail]]$train, feats)
    test_df <- select_post_impute_features(datasets[[detail]]$test, feats)

    set.seed(settings$seed)
    train_df <- undersample_train(
      train_df,
      factor = settings$undersample_factor
    )

    message(sprintf("---TRAINING MODEL FOR %s---", toupper(detail)))
    rf_model <- train_rf(train_df, ntrees = 1000, seed = settings$seed)

    eval <- evaluate_and_plot(rf_model, test_df, top_n = settings$varimp_top_n)

    results[[detail]] <- list(
      model = rf_model,
      evaluation = eval,
      train_df = train_df,
      test_df = test_df
    )
  }

  results
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

undersample_train <- function(train_df, factor = 1) {
  counts <- table(train_df$metagenre)
  min_size <- min(counts)
  max_size <- max(counts)
  max_factor <- max_size / min_size
  if (factor > max_factor) {
    warning("Reducing factor to maximum possible.")
    factor <- max_factor
  }
  target <- floor(min_size * factor)
  parts <- split(train_df, train_df$metagenre)
  sampled <- lapply(parts, function(x) {
    if (nrow(x) > target) {
      x[sample(nrow(x), target), , drop = FALSE]
    } else {
      x
    }
  })
  out <- dplyr::bind_rows(sampled)
  out$metagenre <- factor(out$metagenre, levels = levels(train_df$metagenre))
  out
}

select_post_impute_features <- function(df, features) {
  df |> dplyr::select(dplyr::all_of(features))
}

train_rf <- function(train_df, ntrees = 1000, seed = 42) {
  ranger::ranger(
    formula = metagenre ~ .,
    data = train_df,
    num.trees = ntrees,
    mtry = floor(sqrt(ncol(train_df) - 1)),
    min.node.size = 1,
    importance = "impurity",
    probability = TRUE,
    classification = TRUE,
    seed = seed
  )
}

evaluate_and_plot <- function(model, test_df, top_n = 40) {
  preds <- predict(model, data = test_df)
  pred_classes <- colnames(preds$predictions)[max.col(preds$predictions)]
  all_levels <- union(levels(test_df$metagenre), levels(model$predictions))
  all_levels <- union(levels(test_df$metagenre), levels(model$forest$levels))
  pred_classes <- colnames(preds$predictions)[max.col(preds$predictions)]
  conf <- table(
    Actual = factor(test_df$metagenre, levels = all_levels),
    Predicted = factor(pred_classes, levels = all_levels)
  )

  acc <- sum(diag(conf)) / sum(conf)
  kappa <- caret::confusionMatrix(conf)$overall["Kappa"]
  f1s <- vapply(
    all_levels,
    function(l) {
      tp <- conf[l, l]
      fp <- sum(conf[, l]) - tp
      fn <- sum(conf[l, ]) - tp
      prec <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
      rec <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
      if (prec + rec == 0) 0 else 2 * prec * rec / (prec + rec)
    },
    numeric(1)
  )
  f1macro <- mean(f1s)
  mcc_val <- mltools::mcc(
    actuals = factor(test_df$metagenre, levels = all_levels),
    preds = factor(pred_classes, levels = all_levels)
  )
  metrics <- list(
    accuracy = acc,
    kappa = kappa,
    f1macro = f1macro,
    mcc = mcc_val
  )

  cm_df <- as.data.frame(conf) |>
    dplyr::group_by(Actual) |>
    dplyr::mutate(relfreq = Freq / sum(Freq), labelcolor = relfreq > 0.5) |>
    dplyr::ungroup()

  cm_plot <- plot_cm(cm_df, metrics)

  varimp <- ranger::importance(model)
  varimp_df <- data.frame(Variable = names(varimp), Importance = varimp) |>
    dplyr::arrange(dplyr::desc(Importance))
  varimp_plot <- plot_varimp(varimp_df, top_n = top_n)

  list(
    confusion = cm_df,
    varimp = varimp_df,
    cm_plot = cm_plot,
    varimp_plot = varimp_plot,
    metrics = metrics
  )
}

plot_cm <- function(cm_df, metrics) {
  metrics_text <- sprintf(
    "Acc: %.3f  Kappa: %.3f  F1-macro: %.3f  MCC: %.3f",
    metrics$acc,
    metrics$kappa,
    metrics$f1macro,
    metrics$mcc
  )
  ggplot2::ggplot(
    cm_df,
    ggplot2::aes(x = Predicted, y = forcats::fct_rev(Actual), fill = relfreq)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(label = Freq, color = labelcolor),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#c40d20",
      labels = scales::percent_format(accuracy = 1),
      name = "Rel. freq\n(pred | actual)"
    ) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      subtitle = metrics_text,
      x = "PREDICTED",
      y = "ACTUAL"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(color = "grey45"),
      legend.title = ggplot2::element_text(color = "grey45"),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 0,
        vjust = 0,
        size = 12
      ),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(color = "grey45", size = 14),
      axis.title.y = ggplot2::element_text(color = "grey45", size = 14),
      plot.subtitle = ggplot2::element_text(face = "bold")
    )
}

plot_varimp <- function(varimp_df, top_n = 40) {
  df <- varimp_df |>
    dplyr::arrange(dplyr::desc(Importance)) |>
    head(top_n)
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = reorder(Variable, Importance),
      y = Importance
    )
  ) +
    ggplot2::geom_col(fill = "grey50") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = "Variable",
      y = "Importance"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(color = "grey45"),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(color = "grey45"),
      axis.title.y = ggplot2::element_blank()
    )
  p
}
