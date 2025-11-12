#' Train and Evaluate Baseline Models (tidymodels)
#'
#' Trains baseline models (random assignment and elastic net) for metagenre
#' classification using the tidymodels framework.
#'
#' @param settings List containing seed, n_cores, run flags, model_features,
#'   and glmnet_alpha
#' @param datasets Output from `prepare_rf_data()` with train/test splits and cv_splits
#' @return List with `random` and `glmnet` results for low/high resolution
#' @export
train_and_evaluate_baselines <- function(settings, datasets) {
  set.seed(settings$seed)

  results <- list(random = list(), glmnet = list())

  # Random baselines
  if (isTRUE(settings$run_random_low)) {
    log_info("---TRAINING RANDOM BASELINE FOR LOW---")
    results$random$low <- train_random_baseline(
      datasets$low$train,
      datasets$low$test,
      settings
    )
  }

  if (isTRUE(settings$run_random_high)) {
    log_info("---TRAINING RANDOM BASELINE FOR HIGH---")
    results$random$high <- train_random_baseline(
      datasets$high$train,
      datasets$high$test,
      settings
    )
  }

  # Elastic net baselines
  if (isTRUE(settings$run_glmnet_low)) {
    log_info("---TRAINING GLMNET BASELINE FOR LOW---")
    results$glmnet$low <- train_glmnet_baseline(
      datasets$low,
      settings
    )
  }

  if (isTRUE(settings$run_glmnet_high)) {
    log_info("---TRAINING GLMNET BASELINE FOR HIGH---")
    results$glmnet$high <- train_glmnet_baseline(
      datasets$high,
      settings
    )
  }

  results
}

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

  # Evaluate
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

train_glmnet_baseline <- function(dataset, settings) {
  set.seed(settings$seed)

  vars_to_remove <- setdiff(
    colnames(dataset$train),
    c("metagenre", settings$model_features)
  )

  recipe <- recipes::recipe(metagenre ~ ., data = dataset$train) |>
    recipes::step_rm(dplyr::all_of(vars_to_remove)) |>
    recipes::step_unknown(recipes::all_nominal_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE) |>
    recipes::step_impute_mean(recipes::all_numeric_predictors())

  model_spec <- parsnip::multinom_reg(
    penalty = tune::tune(),
    mixture = settings$glmnet_alpha %||% 0.5
  ) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification")

  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_spec)

  tune_results <- tune::tune_grid(
    workflow,
    resamples = dataset$cv_splits,
    grid = dials::grid_regular(dials::penalty(), levels = 20),
    control = tune::control_grid(parallel_over = "everything"),
    metrics = yardstick::metric_set(yardstick::accuracy)
  )

  best_penalty <- tune::select_best(tune_results, metric = "accuracy")
  final_workflow <- tune::finalize_workflow(workflow, best_penalty)
  final_fit <- parsnip::fit(final_workflow, data = dataset$train)

  train_pred <- predict(final_fit, dataset$train)$.pred_class
  test_pred <- predict(final_fit, dataset$test)$.pred_class

  evaluation <- list(
    train = evaluate_predictions(dataset$train$metagenre, train_pred),
    test = evaluate_predictions(dataset$test$metagenre, test_pred)
  )

  list(
    model = final_fit,
    tune_results = tune_results,
    evaluation = evaluation,
    model_settings = list(
      seed = settings$seed,
      model_type = "glmnet_multinomial",
      features = settings$model_features,
      alpha = settings$glmnet_alpha %||% 0.5,
      best_penalty = best_penalty$penalty
    )
  )
}

evaluate_predictions <- function(true_labels, predicted_labels) {
  all_levels <- union(levels(true_labels), levels(predicted_labels))

  cm <- yardstick::conf_mat(
    tibble::tibble(
      truth = factor(true_labels, levels = all_levels),
      estimate = factor(predicted_labels, levels = all_levels)
    ),
    truth = truth,
    estimate = estimate
  )

  metrics_df <- tibble::tibble(
    truth = factor(true_labels, levels = all_levels),
    estimate = factor(predicted_labels, levels = all_levels)
  )

  metrics <- list(
    accuracy = yardstick::accuracy_vec(metrics_df$truth, metrics_df$estimate),
    kappa = yardstick::kap_vec(metrics_df$truth, metrics_df$estimate),
    f1macro = yardstick::f_meas_vec(
      metrics_df$truth,
      metrics_df$estimate,
      estimator = "macro"
    ),
    mcc = yardstick::mcc_vec(metrics_df$truth, metrics_df$estimate)
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
