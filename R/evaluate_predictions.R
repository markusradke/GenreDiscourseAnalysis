evaluate_predictions <- function(
  true_labels,
  predicted_labels,
  predicted_probs = NULL
) {
  all_levels <- union(levels(true_labels), levels(predicted_labels))

  predictions_df <- create_predictions_df(
    true_labels,
    predicted_labels,
    predicted_probs,
    all_levels
  )

  cm_df <- compute_confusion_matrix(
    true_labels,
    predicted_labels,
    all_levels
  )

  metrics <- if (!is.null(predicted_probs)) {
    calculate_metrics_with_probs(predictions_df)
  } else {
    calculate_metrics_class_only(predictions_df)
  }

  list(cm = cm_df, metrics = metrics)
}

create_predictions_df <- function(
  true_labels,
  predicted_labels,
  predicted_probs,
  all_levels
) {
  base_df <- tibble::tibble(
    metagenre = factor(true_labels, levels = all_levels),
    .pred_class = factor(predicted_labels, levels = all_levels)
  )

  if (!is.null(predicted_probs)) {
    base_df <- dplyr::bind_cols(base_df, predicted_probs)
    base_df <- ensure_prob_cols_complete(base_df, all_levels)
  }

  base_df
}

ensure_prob_cols_complete <- function(predictions_df, truth_levels) {
  pred_cols <- grep("^\\.pred_", names(predictions_df), value = TRUE)
  pred_cols <- pred_cols[pred_cols != ".pred_class"]
  pred_classes <- gsub("^\\.pred_", "", pred_cols)

  missing_classes <- setdiff(truth_levels, pred_classes)

  if (length(missing_classes) > 0) {
    for (missing_class in missing_classes) {
      col_name <- paste0(".pred_", missing_class)
      predictions_df[[col_name]] <- 0
    }
  }

  predictions_df
}

compute_confusion_matrix <- function(
  true_labels,
  predicted_labels,
  all_levels
) {
  conf_table <- table(
    Actual = factor(true_labels, levels = all_levels),
    Predicted = factor(predicted_labels, levels = all_levels)
  )

  as.data.frame(conf_table) |>
    dplyr::group_by(Actual) |>
    dplyr::mutate(
      relfreq = Freq / sum(Freq),
      labelcolor = relfreq > 0.5
    ) |>
    dplyr::ungroup()
}

calculate_metrics_class_only <- function(predictions_df) {
  metric_set <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    macro_f1_with_zeros,
    yardstick::mcc
  )

  metrics_result <- metric_set(
    predictions_df,
    truth = metagenre,
    estimate = .pred_class
  )

  extract_metric_values(metrics_result, include_log_loss = FALSE)
}

calculate_metrics_with_probs <- function(predictions_df) {
  metric_set <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    macro_f1_with_zeros,
    yardstick::mcc,
    yardstick::mn_log_loss
  )

  metrics_result <- metric_set(
    predictions_df,
    truth = metagenre,
    estimate = .pred_class,
    dplyr::matches(".pred_(?!class$)", perl = TRUE)
  )

  extract_metric_values(metrics_result, include_log_loss = TRUE)
}

extract_metric_values <- function(metrics_result, include_log_loss) {
  metrics_list <- setNames(
    as.list(metrics_result$.estimate),
    metrics_result$.metric
  )

  result <- list(
    accuracy = metrics_list$accuracy,
    kappa = metrics_list$kap,
    f1macro = metrics_list$macro_f1_with_zeros,
    mcc = metrics_list$mcc
  )

  if (include_log_loss) {
    result$mn_log_loss <- metrics_list$mn_log_loss
  }

  result
}
