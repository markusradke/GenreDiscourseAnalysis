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

  cv_splits <- dataset$cv_splits # extract to reduce size for parallel export

  tune_results <- tune_penalty_glmnet_parallel(
    workflow,
    cv_splits,
    settings$n_cores
  )

  best_penalty <- tune::select_best(
    tune_results,
    metric = "macro_f1_with_zeros"
  )
  final_workflow <- tune::finalize_workflow(workflow, best_penalty)
  final_fit <- parsnip::fit(final_workflow, data = dataset$train)

  train_pred <- predict(final_fit, dataset$train)$.pred_class
  test_pred <- predict(final_fit, dataset$test)$.pred_class
  message("Parallel backend unregistered.")

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
      alpha = settings$glmnet_alpha,
      best_penalty = best_penalty$penalty
    )
  )
}


tune_penalty_glmnet_parallel <- function(workflow, cv_splits, n_cores) {
  options(future.globals.maxSize = 2L * 1024^3) # 2GB
  future::plan(future::multisession, workers = n_cores)
  on.exit({
    future::plan(future::sequential)
    options(future.globals.maxSize = 500L * 1024^2) # back to 500MB
  })
  log_info(
    paste0(
      "Tuning with max.",
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
