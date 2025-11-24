train_glmnet <- function(dataset, settings) {
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
  # TODO standardisieren

  model_spec <- parsnip::multinom_reg(
    penalty = tune::tune(),
    mixture = settings$glmnet_alpha
  ) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification")

  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_spec)

  message(
    "Starting hyperparameter tuning for glmnet, registering parallel backend..."
  )

  start_time <- Sys.time()
  cv_splits <- dataset$cv_splits # extract to reduce size for parallel export
  tuning_results <- tune_penalty_glmnet_parallel(
    workflow,
    cv_splits,
    settings$n_cores
  )

  best_penalty <- tune::select_best(
    tuning_results,
    metric = "macro_f1_with_zeros"
  )
  final_workflow <- tune::finalize_workflow(workflow, best_penalty)
  final_fit <- parsnip::fit(final_workflow, data = dataset$train)
  message("Parallel backend unregistered.")

  evaluation <- evaluate_glmnet_model(
    final_fit,
    dataset$train,
    dataset$test
  )
  end_time <- Sys.time()
  time_needed <- end_time - start_time
  evaluation$time_needed <- time_needed

  list(
    model = final_fit,
    tuning_results = tuning_results,
    evaluation = evaluation,
    model_settings = list(
      seed = settings$seed,
      model_type = "glmnet_multinomial",
      features = settings$model_features,
      alpha = settings$glmnet_alpha,
      best_penalty = best_penalty$penalty
    )
  )
}

evaluate_glmnet_model <- function(fitted_model, train_df, test_df) {
  train_eval <- compute_glmnet_predictions(fitted_model, train_df)
  test_eval <- compute_glmnet_predictions(fitted_model, test_df)

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics
  )
}

compute_glmnet_predictions <- function(fitted_model, df) {
  pred_class <- predict(fitted_model, df)$.pred_class
  pred_prob <- predict(fitted_model, df, type = "prob")

  evaluate_predictions(
    true_labels = df$metagenre,
    predicted_labels = pred_class,
    predicted_probs = pred_prob
  )
}

tune_penalty_glmnet_parallel <- function(workflow, cv_splits, n_cores) {
  options(future.globals.maxSize = 2L * 1024^3) # 2GB
  future::plan(future::multisession, workers = n_cores)
  on.exit({
    future::plan(future::sequential)
    options(future.globals.maxSize = 500L * 1024^2) # back to 500MB
  })
  message(
    paste0(
      "Tuning with max. ",
      future::nbrOfWorkers(),
      " parallel workers..."
    )
  )
  tune::tune_grid(
    workflow,
    resamples = cv_splits,
    grid = dials::grid_regular(dials::penalty(), levels = 20),
    control = tune::control_grid(
      parallel_over = "everything",
      verbose = TRUE,
      allow_par = TRUE
    ),
    metrics = yardstick::metric_set(
      macro_f1_with_zeros,
      yardstick::accuracy,
      yardstick::kap,
      yardstick::mcc
    )
  )
}
