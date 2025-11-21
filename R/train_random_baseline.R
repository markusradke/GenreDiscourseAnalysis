train_random_baseline <- function(train_data, test_data, settings) {
  set.seed(settings$seed)

  props <- calculate_class_proportions(train_data$metagenre)
  evaluation <- evaluate_random_baseline(train_data, test_data, props)

  list(
    model = list(type = "random", class_proportions = props),
    evaluation = evaluation,
    model_settings = list(
      seed = settings$seed,
      model_type = "random_baseline"
    )
  )
}

calculate_class_proportions <- function(metagenre) {
  table(metagenre) / length(metagenre)
}

evaluate_random_baseline <- function(train_df, test_df, class_props) {
  train_eval <- compute_random_predictions(train_df, class_props)
  test_eval <- compute_random_predictions(test_df, class_props)

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics
  )
}

compute_random_predictions <- function(df, class_props) {
  levels_vec <- names(class_props)

  pred_labels <- factor(
    sample(
      levels_vec,
      nrow(df),
      replace = TRUE,
      prob = class_props
    ),
    levels = levels_vec
  )

  evaluate_predictions(
    true_labels = df$metagenre,
    predicted_labels = pred_labels,
    predicted_probs = NULL
  )
}
