#' Macro F1 with zero-filling for unpredicted classes
#'
#' Calculates macro-averaged F1, setting F1 = 0 for classes that were never
#' predicted (even if they exist in the data). This penalizes the model for
#' completely failing on minority classes.
#'
#' @param data A data frame with `truth` and `estimate` columns
#' @param truth The column identifier for the true class results (unquoted).
#'   Defaults to `truth`.
#' @param estimate The column identifier for the predicted class results
#'   (unquoted). Defaults to `estimate`.
#' @param estimator Not used, always "macro".
#' @param na_rm Logical. Should missing values be removed?
#' @param event_level Not used for multiclass.
#' @param case_weights Not currently used.
#' @param ... Not currently used.
#' @return A tibble with columns `.metric`, `.estimator`, and `.estimate`
#'
#' @export
macro_f1_with_zeros <- function(
  data,
  truth = truth,
  estimate = estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = "first",
  case_weights = NULL,
  ...
) {
  yardstick::class_metric_summarizer(
    name = "macro_f1_with_zeros",
    fn = function(
      truth,
      estimate,
      estimator = NULL,
      na_rm = TRUE,
      event_level = "first",
      case_weights = NULL,
      ...
    ) {
      all_levels <- union(levels(truth), levels(estimate))
      truth <- factor(truth, levels = all_levels)
      estimate <- factor(estimate, levels = all_levels)

      f1_per_class <- vapply(
        all_levels,
        function(class) {
          tp <- sum(truth == class & estimate == class, na.rm = na_rm)
          fp <- sum(truth != class & estimate == class, na.rm = na_rm)
          fn <- sum(truth == class & estimate != class, na.rm = na_rm)

          if (tp == 0 && fp == 0 && fn == 0) {
            return(NA_real_)
          }

          if (tp == 0 && (fp > 0 || fn > 0)) {
            return(0)
          }

          precision <- tp / (tp + fp)
          recall <- tp / (tp + fn)
          2 * (precision * recall) / (precision + recall)
        },
        FUN.VALUE = numeric(1)
      )

      f1_per_class <- f1_per_class[!is.na(f1_per_class)]
      mean(f1_per_class, na.rm = na_rm)
    },
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    estimator = "macro",
    na_rm = na_rm,
    event_level = event_level,
    case_weights = {{ case_weights }},
    ...
  )
}

macro_f1_with_zeros <- yardstick::new_class_metric(
  macro_f1_with_zeros,
  "maximize"
)
