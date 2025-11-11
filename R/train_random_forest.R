#' Train and evaluate random-forest models for MusicBrainz with low and high metagenre detail
#'
#' Selects the post-imputation features, optionally undersamples, trains
#' a ranger random-forest and evaluates/plots results for each set of metagenres.
#'
#' @param settings Named list of pipeline settings (must contain \code{features_after_impute},
#'   \code{undersample_factor}, \code{varimp_top_n}, \code{seed}, \code{run_rf_low}, \code{run_rf_high},
#'  \code{ntrees}, \code{mtry}, \code{min.node.size}, \code{max depth}).
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
    rf_model <- train_rf(
      train_df,
      ntrees = settings$ntrees,
      mtry = settings$mtry,
      min.node.size = settings$min.node.size,
      max.depth = settings$max.depth,
      seed = settings$seed
    )

    model_settings <- get_model_settings(rf_model)

    eval <- evaluate_train_and_test(
      rf_model,
      train_df,
      test_df,
      top_n = settings$varimp_top_n
    )

    results[[detail]] <- list(
      model = rf_model,
      model_settings = model_settings,
      evaluation = eval,
      train_df = train_df,
      test_df = test_df
    )
  }

  results
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

train_rf <- function(
  train_df,
  ntrees,
  mtry,
  min.node.size,
  max.depth,
  seed = 42
) {
  ranger::ranger(
    formula = metagenre ~ .,
    data = train_df,
    num.trees = ntrees,
    mtry = mtry,
    max.depth = max.depth,
    min.node.size = min.node.size,
    importance = "impurity",
    probability = TRUE,
    classification = TRUE,
    seed = seed
  )
}

get_model_settings <- function(model) {
  list(
    ntrees = model$num.trees,
    mtry = model$mtry,
    max.depth = model$max.depth,
    min.node.size = model$min.node.size,
    nindependent = model$num.independent.variables,
    vip.mode = model$importance.mode,
    splitrule = model$splitrule,
    treetype = model$treetype
  )
}

evaluate_train_and_test <- function(model, train_df, test_df, top_n = 40) {
  train_eval <- get_cm_and_metrics(model, train_df)
  test_eval <- get_cm_and_metrics(model, test_df)

  varimp <- ranger::importance(model)
  varimp_df <- data.frame(Variable = names(varimp), Importance = varimp) |>
    dplyr::arrange(dplyr::desc(Importance))

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics,
    varimp = varimp_df
  )
}

get_cm_and_metrics <- function(model, df) {
  preds <- predict(model, data = df)
  pred_classes <- colnames(preds$predictions)[max.col(preds$predictions)]
  all_levels <- union(levels(df$metagenre), levels(model$predictions))
  all_levels <- union(levels(df$metagenre), levels(model$forest$levels))
  pred_classes <- colnames(preds$predictions)[max.col(preds$predictions)]
  conf <- table(
    Actual = factor(df$metagenre, levels = all_levels),
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
    actuals = factor(df$metagenre, levels = all_levels),
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

  list(cm = cm_df, metrics = metrics)
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
