create_minimal_rf_data <- function(n = 200, n_classes = 3) {
  set.seed(42)
  classes <- paste0("genre_", seq_len(n_classes))

  train_df <- data.frame(
    metagenre = factor(
      sample(classes, n, replace = TRUE),
      levels = classes
    ),
    feature_1 = stats::rnorm(n),
    feature_2 = stats::rnorm(n),
    feature_3 = stats::rnorm(n)
  )

  test_df <- data.frame(
    metagenre = factor(
      sample(classes, n / 2, replace = TRUE),
      levels = classes
    ),
    feature_1 = stats::rnorm(n / 2),
    feature_2 = stats::rnorm(n / 2),
    feature_3 = stats::rnorm(n / 2)
  )

  list(train = train_df, test = test_df)
}

create_minimal_cv_splits <- function(train_df, n_folds = 3) {
  rsample::vfold_cv(train_df, v = n_folds, strata = "metagenre")
}

create_minimal_settings <- function(tune = FALSE) {
  list(
    features_after_impute = c("feature_1", "feature_2", "feature_3"),
    under_ratio = 2,
    varimp_top_n = 3,
    seed = 42,
    run_rf_low = TRUE,
    run_rf_high = FALSE,
    tune_hyperparameters = tune,
    n_cores = 1,
    ntrees = 10,
    ntrees_tuning = 5,
    mtry = 2,
    min.node.size = 5,
    importance = "impurity",
    initial_grid_density = 2,
    bayes_iterations = 2,
    uncertain_jump = 1
  )
}

test_that("get_class_weights computes inverse frequency weights", {
  metagenres <- factor(c(
    rep("A", 100),
    rep("B", 50),
    rep("C", 25)
  ))

  weights <- get_class_weights(metagenres)

  expect_type(weights, "double")
  expect_length(weights, 3)
  expect_true(all(weights > 0))
  expect_true(weights["C"] > weights["B"])
  expect_true(weights["B"] > weights["A"])
})

test_that("get_class_weights uses sqrt to moderate extremes", {
  metagenres <- factor(c(rep("A", 1000), rep("B", 10)))

  weights <- get_class_weights(metagenres)

  ratio_raw <- 1000 / 10
  ratio_weighted <- weights["B"] / weights["A"]

  expect_lt(ratio_weighted, ratio_raw)
  expect_gt(ratio_weighted, sqrt(ratio_raw) * 0.9)
})

test_that("get_class_weights handles single class", {
  metagenres <- factor(rep("A", 100))

  weights <- get_class_weights(metagenres)

  expect_length(weights, 1)
  expect_equal(as.numeric(weights), 1)
})

test_that("extract_ranger_model returns ranger object", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42)
  fitted <- parsnip::fit(workflow, data = data$train)

  ranger_model <- extract_ranger_model(fitted)

  expect_s3_class(ranger_model, "ranger")
  expect_true("num.trees" %in% names(ranger_model))
})

test_that("extract_model_settings returns complete list", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42)
  fitted <- parsnip::fit(workflow, data = data$train)
  ranger_model <- extract_ranger_model(fitted)

  settings <- extract_model_settings(ranger_model)

  expect_type(settings, "list")
  expect_named(
    settings,
    c(
      "ntrees",
      "mtry",
      "max.depth",
      "min.node.size",
      "nindependent",
      "vip.mode",
      "splitrule",
      "treetype"
    )
  )
  expect_equal(settings$ntrees, 10)
  expect_equal(settings$nindependent, 3)
})

test_that("create_rf_workflow produces valid workflow", {
  data <- create_minimal_rf_data()

  workflow <- create_rf_workflow(
    train_df = data$train,
    tune_hyperparameters = FALSE,
    ntrees = 10,
    seed = 42
  )

  expect_s3_class(workflow, "workflow")
  expect_true(workflows::is_trained_workflow(workflow) == FALSE)
})

test_that("create_rf_workflow includes downsampling step", {
  data <- create_minimal_rf_data()

  workflow <- create_rf_workflow(data$train, under_ratio = 2, seed = 42)

  recipe_steps <- workflow |>
    workflows::extract_preprocessor() |>
    purrr::pluck("steps")

  has_downsample <- any(
    purrr::map_lgl(recipe_steps, ~ inherits(.x, "step_downsample"))
  )

  expect_true(has_downsample)
})

test_that("create_rf_workflow with tuning uses tune placeholders", {
  data <- create_minimal_rf_data()

  workflow <- create_rf_workflow(
    train_df = data$train,
    tune_hyperparameters = TRUE,
    seed = 42
  )

  params <- hardhat::extract_parameter_set_dials(workflow)

  expect_gt(nrow(params), 0)
  expect_true("mtry" %in% params$id)
  expect_true("min_n" %in% params$id)
})

test_that("create_rf_workflow without tuning uses fixed params", {
  data <- create_minimal_rf_data()

  workflow <- create_rf_workflow(
    train_df = data$train,
    tune_hyperparameters = FALSE,
    mtry = 2,
    min.node.size = 10,
    seed = 42
  )

  model_spec <- workflows::extract_spec_parsnip(workflow)

  expect_equal(model_spec$args$mtry, 2)
  expect_equal(model_spec$args$min_n, 10)
})

test_that("fit_workflow returns fitted workflow", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42)

  result <- fit_workflow(workflow, data$train, n_cores = 1)

  expect_type(result, "list")
  expect_named(result, c("fitted_workflow", "tuning_results"))
  expect_true(workflows::is_trained_workflow(result$fitted_workflow))
  expect_null(result$tuning_results)
})

test_that("fit_workflow handles case weights column", {
  data <- create_minimal_rf_data()
  data$train$case_wts <- hardhat::importance_weights(
    stats::runif(nrow(data$train), 0.5, 1)
  )
  workflow <- create_rf_workflow(data$train, seed = 42)

  result <- fit_workflow(workflow, data$train, n_cores = 1)

  expect_true(workflows::is_trained_workflow(result$fitted_workflow))
})

test_that("compute_confusion_and_metrics returns structure", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42)
  fitted <- parsnip::fit(workflow, data = data$train)

  result <- compute_confusion_and_metrics(fitted, data$test)

  expect_type(result, "list")
  expect_named(result, c("cm", "metrics"))
  expect_s3_class(result$cm, "data.frame")
  expect_type(result$metrics, "list")
})

test_that("compute_confusion_and_metrics has confusion matrix", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42)
  fitted <- parsnip::fit(workflow, data = data$train)

  result <- compute_confusion_and_metrics(fitted, data$test)

  expect_true("Actual" %in% names(result$cm))
  expect_true("Predicted" %in% names(result$cm))
  expect_true("Freq" %in% names(result$cm))
  expect_true("relfreq" %in% names(result$cm))
})

test_that("compute_confusion_and_metrics adds missing prob cols", {
  data <- create_minimal_rf_data(n = 200, n_classes = 4)
  data$train <- data$train |>
    dplyr::filter(.data$metagenre != "genre_4")

  workflow <- create_rf_workflow(data$train, seed = 42)
  fitted <- parsnip::fit(workflow, data = data$train)

  expect_message(
    result <- compute_confusion_and_metrics(fitted, data$test),
    "Adding zero probability columns"
  )

  expect_type(result, "list")
})

test_that("calculate_yardstick_metrics returns all metrics", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42)
  fitted <- parsnip::fit(workflow, data = data$train)

  preds_class <- stats::predict(fitted, new_data = data$test, type = "class")
  preds_prob <- stats::predict(fitted, new_data = data$test, type = "prob")
  preds_df <- dplyr::bind_cols(
    data$test |> dplyr::select("metagenre"),
    preds_class,
    preds_prob
  )

  metrics <- calculate_yardstick_metrics(preds_df)

  expect_named(
    metrics,
    c("accuracy", "kappa", "f1macro", "mcc", "mn_log_loss")
  )
  expect_type(metrics$accuracy, "double")
  expect_gte(metrics$accuracy, 0)
  expect_lte(metrics$accuracy, 1)
})

test_that("evaluate_with_metrics returns complete evaluation", {
  data <- create_minimal_rf_data()
  workflow <- create_rf_workflow(data$train, seed = 42, importance = "impurity")
  fitted <- parsnip::fit(workflow, data = data$train)

  eval <- evaluate_with_metrics(fitted, data$train, data$test, varimp_top_n = 3)

  expect_type(eval, "list")
  expect_named(
    eval,
    c(
      "confusion_train",
      "metrics_train",
      "confusion_test",
      "metrics_test",
      "varimp"
    )
  )
  expect_s3_class(eval$varimp, "data.frame")
  expect_true("Variable" %in% names(eval$varimp))
  expect_true("Importance" %in% names(eval$varimp))
})

test_that("train_and_evaluate_rf_tidy validates settings", {
  data <- create_minimal_rf_data()
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )

  invalid_settings <- list(features_after_impute = NULL)

  expect_error(
    train_and_evaluate_rf_tidy(invalid_settings, datasets),
    "Please provide settings\\$features_after_impute"
  )
})

test_that("train_and_evaluate_rf_tidy adds metagenre to features", {
  data <- create_minimal_rf_data()
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings()

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  expect_true("metagenre" %in% names(result$low$train_df))
})

test_that("train_and_evaluate_rf_tidy skips disabled models", {
  data <- create_minimal_rf_data()
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL),
    high = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings()
  settings$run_rf_high <- FALSE

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  expect_null(result$high)
  expect_type(result$low, "list")
})

test_that("train_and_evaluate_rf_tidy without tuning works", {
  data <- create_minimal_rf_data()
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings(tune = FALSE)

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  expect_type(result, "list")
  expect_named(result, "low")
  expect_s3_class(result$low$model, "ranger")
  expect_type(result$low$evaluation, "list")
  expect_null(result$low$tuning_results)
})

test_that("train_and_evaluate_rf_tidy returns complete structure", {
  data <- create_minimal_rf_data()
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings()

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  expect_named(
    result$low,
    c(
      "model",
      "model_settings",
      "evaluation",
      "train_df",
      "test_df",
      "tuning_results"
    )
  )
  expect_s3_class(result$low$model, "ranger")
  expect_type(result$low$model_settings, "list")
  expect_type(result$low$evaluation, "list")
  expect_s3_class(result$low$train_df, "data.frame")
  expect_s3_class(result$low$test_df, "data.frame")
})

test_that("train_and_evaluate_rf_tidy detects case weights", {
  data <- create_minimal_rf_data()
  data$train$case_wts <- hardhat::importance_weights(
    stats::runif(nrow(data$train), 0.5, 1)
  )
  data$test$case_wts <- hardhat::importance_weights(
    stats::runif(nrow(data$test), 0.5, 1)
  )
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings()

  expect_message(
    result <- train_and_evaluate_rf_tidy(settings, datasets),
    "Case weights detected"
  )

  expect_s3_class(result$low$model, "ranger")
})

test_that("tune_and_fit_workflow performs grid search", {
  data <- create_minimal_rf_data(n = 300)
  cv_splits <- create_minimal_cv_splits(data$train, n_folds = 3)
  workflow <- create_rf_workflow(
    data$train,
    tune_hyperparameters = TRUE,
    seed = 42
  )

  result <- tune_and_fit_workflow(
    workflow = workflow,
    train_df = data$train,
    cv_splits = cv_splits,
    seed = 42,
    n_cores = 1,
    initial_grid_density = 2,
    bayes_iterations = 2,
    uncertain_jump = 1,
    ntrees_final = 10
  )

  expect_type(result, "list")
  expect_named(result, c("fitted_workflow", "tuning_results"))
  expect_true(workflows::is_trained_workflow(result$fitted_workflow))
  expect_s3_class(result$tuning_results, "tune_results")
})

test_that("tune_and_fit_workflow selects best parameters", {
  data <- create_minimal_rf_data(n = 300)
  cv_splits <- create_minimal_cv_splits(data$train, n_folds = 3)
  workflow <- create_rf_workflow(
    data$train,
    tune_hyperparameters = TRUE,
    seed = 42
  )

  expect_message(
    result <- tune_and_fit_workflow(
      workflow = workflow,
      train_df = data$train,
      cv_splits = cv_splits,
      seed = 42,
      n_cores = 1,
      initial_grid_density = 2,
      bayes_iterations = 2,
      uncertain_jump = 1,
      ntrees_final = 10
    ),
    "Best hyperparameters"
  )

  expect_true(workflows::is_trained_workflow(result$fitted_workflow))
})

test_that("tune_and_fit_workflow uses final ntrees", {
  data <- create_minimal_rf_data(n = 300)
  cv_splits <- create_minimal_cv_splits(data$train, n_folds = 3)
  workflow <- create_rf_workflow(
    data$train,
    tune_hyperparameters = TRUE,
    ntrees_tuning = 5,
    seed = 42
  )

  result <- tune_and_fit_workflow(
    workflow = workflow,
    train_df = data$train,
    cv_splits = cv_splits,
    seed = 42,
    n_cores = 1,
    initial_grid_density = 2,
    bayes_iterations = 2,
    uncertain_jump = 1,
    ntrees_final = 20
  )

  ranger_model <- extract_ranger_model(result$fitted_workflow)
  expect_equal(ranger_model$num.trees, 20)
})

test_that("train_and_evaluate_rf_tidy with tuning works", {
  data <- create_minimal_rf_data(n = 300)
  cv_splits <- create_minimal_cv_splits(data$train, n_folds = 3)
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = cv_splits)
  )
  settings <- create_minimal_settings(tune = TRUE)

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  expect_type(result, "list")
  expect_s3_class(result$low$model, "ranger")
  expect_s3_class(result$low$tuning_results, "tune_results")
})

test_that("integration: full pipeline without tuning", {
  data <- create_minimal_rf_data(n = 200)
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings(tune = FALSE)

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  expect_s3_class(result$low$model, "ranger")
  expect_gt(result$low$evaluation$metrics_test$accuracy, 0)
  expect_lte(result$low$evaluation$metrics_test$accuracy, 1)
  expect_gt(nrow(result$low$evaluation$confusion_test), 0)
  expect_equal(nrow(result$low$evaluation$varimp), 3)
})

test_that("integration: full pipeline with case weights", {
  data <- create_minimal_rf_data(n = 200)
  data$train$case_wts <- hardhat::importance_weights(
    stats::runif(nrow(data$train), 0.6, 1)
  )
  data$test$case_wts <- hardhat::importance_weights(
    stats::runif(nrow(data$test), 0.6, 1)
  )
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings(tune = FALSE)

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  ranger_model <- result$low$model
  expect_true(!is.null(ranger_model$case.weights))
  expect_length(ranger_model$case.weights, nrow(data$train))
})

test_that("integration: predictions work on new data", {
  data <- create_minimal_rf_data(n = 200)
  datasets <- list(
    low = list(train = data$train, test = data$test, cv_splits = NULL)
  )
  settings <- create_minimal_settings(tune = FALSE)
  result <- train_and_evaluate_rf_tidy(settings, datasets)

  new_data <- data.frame(
    feature_1 = stats::rnorm(10),
    feature_2 = stats::rnorm(10),
    feature_3 = stats::rnorm(10)
  )

  preds <- stats::predict(
    result$low$model,
    data = new_data,
    type = "response"
  )

  expect_type(preds$predictions, "double")
  expect_equal(nrow(preds$predictions), 10)
  expect_equal(ncol(preds$predictions), 3)
})

test_that("integration: variable importance is meaningful", {
  n <- 500
  set.seed(42)
  data <- data.frame(
    metagenre = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    important_feat = stats::rnorm(n),
    noise_feat = stats::rnorm(n)
  )
  data$important_feat <- data$important_feat +
    as.numeric(data$metagenre) * 2

  test_data <- data[1:100, ]
  train_data <- data[101:500, ]

  datasets <- list(
    low = list(train = train_data, test = test_data, cv_splits = NULL)
  )
  settings <- list(
    features_after_impute = c("important_feat", "noise_feat"),
    under_ratio = 2,
    varimp_top_n = 2,
    seed = 42,
    run_rf_low = TRUE,
    run_rf_high = FALSE,
    tune_hyperparameters = FALSE,
    n_cores = 1,
    ntrees = 50,
    mtry = 2,
    min.node.size = 5,
    importance = "impurity"
  )

  result <- train_and_evaluate_rf_tidy(settings, datasets)

  varimp <- result$low$evaluation$varimp
  important_rank <- which(varimp$Variable == "important_feat")
  noise_rank <- which(varimp$Variable == "noise_feat")

  expect_lt(important_rank, noise_rank)
})
