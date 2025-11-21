train_random_baseline <- function(train_data, test_data, settings) {
  set.seed(settings$seed)

  props <- table(train_data$metagenre) / nrow(train_data)
  levels_vec <- names(props)

  train_pred <- factor(
    sample(levels_vec, nrow(train_data), replace = TRUE, prob = props),
    levels = levels_vec
  )
  test_pred <- factor(
    sample(levels_vec, nrow(test_data), replace = TRUE, prob = props),
    levels = levels_vec
  )

  evaluation <- list(
    train = evaluate_predictions(train_data$metagenre, train_pred),
    test = evaluate_predictions(test_data$metagenre, test_pred)
  )

  list(
    model = list(type = "random", class_proportions = props),
    evaluation = evaluation,
    model_settings = list(seed = settings$seed, model_type = "random_baseline")
  )
}
