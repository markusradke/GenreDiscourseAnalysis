#' Train a multinomial GLMNET classifier for metagenre prediction
#'
#' Train and evaluate a multinomial elastic-net model using tidymodels.
#' Supports optional Bayesian tuning of penalty, mixture and sampling
#' ratios using provided resampling splits. Respects case weights stored
#' in `case_wts`, artist-based CV splits, and a reproducible seed.
#'
#' @param train A tibble/data.frame with training rows. Must contain
#'   columns: `metagenre`, `artist.s.id`, `track.s.id`, `case_wts`
#'   (when `settings$use_caseweights` is TRUE) and the features named in
#'   `settings$model_features`.
#' @param test A tibble/data.frame with hold-out test rows.
#' @param cv_splits Resampling object (e.g. result of
#'   `create_artist_cv_splits()` or `rsample::vfold_cv`) used for tuning.
#'   Can be NULL when no tuning is requested.
#' @param settings List of modelling settings. Required entries include
#'   `seed`, `model_features`, `use_caseweights`, `penalty_fix`,
#'   `alpha_fix`, `tune_penalty`, `tune_alpha`, `tune_downsample`,
#'   `tune_upsample`, `under_ratio_fix`, `over_ratio_fix`, `n_cores`,
#'   `n_cores_tuning`, `initial_grid_size`, `bayes_iterations`,
#'   `uncertain_jump`.
#' @return A list with elements:
#'   - `model`: fitted workflow object (final fitted model)
#'   - `tuning_history`: tuning history tibble or NULL
#'   - `evaluation`: list with confusion matrices and metrics for train
#'       and test sets
#'   - `model_settings`: list with chosen hyperparameters and seed
#' @examples
#' \dontrun{
#' settings <- list(
#'   seed = 42,
#'   model_features = c("f1", "f2"),
#'   use_caseweights = TRUE,
#'   penalty_fix = 1e-4,
#'   alpha_fix = 0.5
#' )
#' out <- train_glmnet(train, test, cv_splits, settings)
#' }
#' @export
train_glmnet <- function(train, test, cv_splits, settings) {
  set.seed(settings$seed)

  vars_to_remove <- setdiff(
    colnames(train),
    c(
      "metagenre",
      "artist.s.id",
      "track.s.id",
      "case_wts",
      settings$model_features
    )
  )

  if (isFALSE(settings$use_caseweights)) {
    train <- train |> dplyr::select(-case_wts)
    cv_splits$splits <- purrr::map(cv_splits$splits, function(split) {
      rsample::make_splits(
        rsample::analysis(split) |> dplyr::select(-case_wts),
        rsample::assessment(split) |> dplyr::select(-case_wts)
      )
    })
  }

  any_tuning <- any_glmnet_hyperparameter_tuned(settings)

  recipe <- create_pre_recipe(
    train,
    vars_to_remove,
    tune_downsample = settings$tune_downsample,
    tune_upsample = settings$tune_upsample,
    under_ratio_fix = settings$under_ratio_fix,
    over_ratio_fix = settings$over_ratio_fix,
    seed = settings$seed
  )

  model_spec <- create_glmnet_model_spec(settings)

  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_spec)
  if (isTRUE(settings$use_caseweights)) {
    workflow <- workflow |>
      workflows::add_case_weights(case_wts)
  }
  print(workflow)

  start_time <- Sys.time()

  if (any_tuning && !is.null(cv_splits)) {
    message("Starting Bayesian hyperparameter tuning for glmnet...")
    glmnet_params <- create_glmnet_params(workflow, train, settings)

    tune_result <- tune_bayes_workflow(
      workflow = workflow,
      train_df = train,
      cv_splits = cv_splits,
      params = glmnet_params,
      model_type = "glmnet",
      seed = settings$seed,
      n_cores_tuning = settings$n_cores_tuning,
      initial_grid_size = settings$initial_grid_size,
      bayes_iterations = settings$bayes_iterations,
      uncertain_jump = settings$uncertain_jump
    )

    tuning_results <- tune_result$tuning_results
    best_params <- tune_result$best_params
    tuning_history <- extract_tuning_history(tuning_results)
  } else {
    message("No tuning requested, fitting with fixed parameters...")
    best_params <- list(
      penalty = settings$penalty_fix,
      mixture = settings$alpha_fix
    )
    tuning_history <- NULL
  }

  final_fit <- finalize_and_fit_glmnet(workflow, train, best_params, settings)

  message("---EVALUATING FINAL GLMNET MODEL---")
  evaluation <- evaluate_glmnet_model(final_fit, train, test)
  end_time <- Sys.time()
  evaluation$time_needed <- end_time - start_time

  list(
    model = final_fit,
    tuning_history = tuning_history,
    evaluation = evaluation,
    model_settings = extract_glmnet_model_settings(settings, best_params)
  )
}

any_glmnet_hyperparameter_tuned <- function(settings) {
  isTRUE(settings$tune_penalty) ||
    isTRUE(settings$tune_alpha) ||
    isTRUE(settings$tune_downsample) ||
    isTRUE(settings$tune_upsample)
}

create_glmnet_model_spec <- function(settings) {
  spec <- parsnip::multinom_reg(
    penalty = settings$penalty_fix,
    mixture = settings$alpha_fix
  ) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification")

  spec <- add_tunable_glmnet_params(spec, settings)
  spec
}

add_tunable_glmnet_params <- function(spec, settings) {
  if (isTRUE(settings$tune_penalty)) {
    spec <- parsnip::set_args(spec, penalty = tune::tune())
  }
  if (isTRUE(settings$tune_alpha)) {
    spec <- parsnip::set_args(spec, mixture = tune::tune())
  }
  spec
}

#' Create glmnet parameter definitions for tuning
#' @param workflow Workflow to extract base params from
#' @param train_df Training data for sampling ratio bounds
#' @param settings Settings list with tune switches
#' @return dials parameter set
create_glmnet_params <- function(workflow, train_df, settings) {
  sampling_params <- create_sampling_params(train_df)

  params <- workflow |>
    hardhat::extract_parameter_set_dials()

  if (isTRUE(settings$tune_penalty)) {
    params <- params |> update(penalty = dials::penalty())
  }

  if (isTRUE(settings$tune_alpha)) {
    params <- params |> update(mixture = dials::mixture())
  }

  if (isTRUE(settings$tune_downsample)) {
    params <- params |> update(under_ratio = sampling_params$under_ratio)
  }

  if (isTRUE(settings$tune_upsample)) {
    params <- params |> update(over_ratio = sampling_params$over_ratio)
  }

  params
}

finalize_and_fit_glmnet <- function(workflow, train_df, best_params, settings) {
  best_penalty <- best_params$penalty %||% settings$penalty_fix
  best_alpha <- best_params$mixture %||% settings$alpha_fix

  final_spec <- parsnip::multinom_reg(
    penalty = best_penalty,
    mixture = best_alpha
  ) |>
    parsnip::set_engine("glmnet", nthreads = settings$n_cores) |>
    parsnip::set_mode("classification")

  finalize_and_fit(
    workflow,
    train_df,
    best_params,
    final_spec = final_spec,
    n_cores = settings$n_cores
  )
}

extract_glmnet_model_settings <- function(settings, best_params) {
  list(
    seed = settings$seed,
    model_type = "glmnet_multinomial",
    features = settings$model_features,
    alpha = best_params$mixture %||% settings$alpha_fix,
    penalty = best_params$penalty %||% settings$penalty_fix,
    under_ratio = best_params$under_ratio %||% settings$under_ratio_fix,
    over_ratio = best_params$over_ratio %||% settings$over_ratio_fix
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
