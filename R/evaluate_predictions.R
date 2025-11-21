evaluate_predictions <- function(true_labels, predicted_labels) {
  all_levels <- union(levels(true_labels), levels(predicted_labels))

  metrics_df <- tibble::tibble(
    truth = factor(true_labels, levels = all_levels),
    estimate = factor(predicted_labels, levels = all_levels)
  )

  # Use metric_set for consistency with tuning
  metric_set <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    macro_f1_with_zeros,
    yardstick::mcc
  )

  metrics_result <- metric_set(
    metrics_df,
    truth = truth,
    estimate = estimate
  )

  metrics <- list(
    accuracy = metrics_result$.estimate[metrics_result$.metric == "accuracy"],
    kappa = metrics_result$.estimate[metrics_result$.metric == "kap"],
    f1macro = metrics_result$.estimate[
      metrics_result$.metric == "macro_f1_with_zeros"
    ],
    mcc = metrics_result$.estimate[metrics_result$.metric == "mcc"]
  )

  # Create confusion matrix
  cm <- yardstick::conf_mat(
    metrics_df,
    truth = truth,
    estimate = estimate
  )

  cm_df <- as.data.frame(cm$table) |>
    dplyr::group_by(Truth) |>
    dplyr::rename(Actual = Truth, Predicted = Prediction) |>
    dplyr::mutate(
      relfreq = Freq / sum(Freq),
      labelcolor = relfreq > 0.5
    ) |>
    dplyr::ungroup()

  list(confusion_matrix = cm_df, metrics = metrics)
}

log_info <- function(msg) cat(msg, "\n")
