#' Train a Regularized Discriminant Analysis (RDA) classifier
#'
#' Train and evaluate a Regularized Discriminant Analysis (RDA) model
#' using the klaR implementation. Supports optional Bayesian tuning of
#' `gamma` and `lambda` regularization parameters and sampling-ratio
#' tuning. Conforms to the project's case-weight and CV conventions.
#'
#' @param train tibble/data.frame with training rows. Must contain
#'   `metagenre`, `artist.s.id`, `track.s.id`, `case_wts` (if
#'   `settings$use_caseweights` TRUE) and features in
#'   `settings$model_features`.
#' @param test tibble/data.frame with hold-out test rows.
#' @param cv_splits resampling object used for tuning (can be NULL).
#' @param settings list of settings. Expected entries include `seed`,
#'   `model_features`, `use_caseweights`, `n_cores`, `n_cores_tuning`,
#'   `tune_gamma`, `tune_lambda`, `gamma_fix`, `lambda_fix`,
#'   `tune_downsample`, `tune_upsample`, and Bayesian tuning params.
#' @return A list with elements: `model`, `tuning_history`, `evaluation`,
#'   `model_settings`.
#' @examples
#' \dontrun{
#' settings <- list(
#'   seed = 42, model_features = c("f1","f2"),
#'   use_caseweights = TRUE,
#'   tune_gamma = TRUE, tune_lambda = TRUE,
#'   gamma_fix = 0.5, lambda_fix = 0.5
#' )
#' out <- train_rda(train, test, cv_splits, settings)
#' }
#' @export
train_rda <- function(train, test, cv_splits, settings) {
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

  any_tuning <- any_rda_hyperparameter_tuned(settings)

  recipe <- create_pre_recipe(
    train,
    vars_to_remove,
    tune_downsample = settings$tune_downsample,
    tune_upsample = settings$tune_upsample,
    under_ratio_fix = settings$under_ratio_fix,
    over_ratio_fix = settings$over_ratio_fix,
    seed = settings$seed
  )

  model_spec <- create_rda_model_spec(settings)

  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_spec)
  if (isTRUE(settings$use_caseweights)) {
    workflow <- workflow |> workflows::add_case_weights(case_wts)
  }

  start_time <- Sys.time()

  if (any_tuning && !is.null(cv_splits)) {
    message("Starting Bayesian hyperparameter tuning for RDA...")
    rda_params <- create_rda_params(workflow, train, settings)

    tune_result <- tune_bayes_workflow(
      workflow = workflow,
      train_df = train,
      cv_splits = cv_splits,
      params = rda_params,
      model_type = "rda",
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
    message("No tuning requested, fitting RDA with fixed params...")
    best_params <- list(
      gamma = settings$gamma_fix,
      lambda = settings$lambda_fix
    )
    tuning_history <- NULL
  }

  final_fit <- finalize_and_fit_rda(workflow, train, best_params, settings)

  message("---EVALUATING FINAL RDA MODEL---")
  evaluation <- evaluate_rda_model(final_fit, train, test)
  end_time <- Sys.time()
  evaluation$time_needed <- end_time - start_time

  list(
    model = final_fit,
    tuning_history = tuning_history,
    evaluation = evaluation,
    model_settings = extract_rda_model_settings(settings, best_params)
  )
}

any_rda_hyperparameter_tuned <- function(settings) {
  isTRUE(settings$tune_gamma) ||
    isTRUE(settings$tune_lambda) ||
    isTRUE(settings$tune_downsample) ||
    isTRUE(settings$tune_upsample)
}

create_rda_model_spec <- function(settings) {
  # Use parsnip discriminant model and klaR engine if available.
  # We set default gamma/lambda from settings and mark tunable args.
  spec <- parsnip::discrim_regularized(
    frac_common = 1 # placeholder; gamma/lambda will be set via args
  ) |>
    parsnip::set_engine("klaR") |>
    parsnip::set_mode("classification")

  spec <- add_tunable_rda_params(spec, settings)
  spec
}

add_tunable_rda_params <- function(spec, settings) {
  # set_args accepts tunable parameters if requested
  if (isTRUE(settings$tune_gamma)) {
    spec <- parsnip::set_args(spec, gamma = tune::tune())
  } else {
    spec <- parsnip::set_args(spec, gamma = settings$gamma_fix)
  }

  if (isTRUE(settings$tune_lambda)) {
    spec <- parsnip::set_args(spec, lambda = tune::tune())
  } else {
    spec <- parsnip::set_args(spec, lambda = settings$lambda_fix)
  }

  spec
}

#' Create RDA parameter definitions for tuning
#' @param workflow Workflow to extract base params from
#' @param train_df Training data for sampling ratio bounds
#' @param settings Settings list with tune switches
#' @return dials parameter set
create_rda_params <- function(workflow, train_df, settings) {
  sampling_params <- create_sampling_params(train_df)

  params <- workflow |> hardhat::extract_parameter_set_dials()

  if (isTRUE(settings$tune_gamma)) {
    params <- params |> update(gamma = dials::regularization_factor()) # numeric 0-1
  }

  if (isTRUE(settings$tune_lambda)) {
    params <- params |> update(lambda = dials::regularization_factor()) # numeric 0-1
  }

  if (isTRUE(settings$tune_downsample)) {
    params <- params |> update(under_ratio = sampling_params$under_ratio)
  }

  if (isTRUE(settings$tune_upsample)) {
    params <- params |> update(over_ratio = sampling_params$over_ratio)
  }

  params
}

finalize_and_fit_rda <- function(workflow, train_df, best_params, settings) {
  best_gamma <- best_params$gamma %||% settings$gamma_fix
  best_lambda <- best_params$lambda %||% settings$lambda_fix

  final_spec <- parsnip::discrim_regularized(
    frac_common = 1
  ) |>
    parsnip::set_engine("klaR", gamma = best_gamma, lambda = best_lambda) |>
    parsnip::set_mode("classification")

  finalize_and_fit(
    workflow,
    train_df,
    best_params,
    final_spec = final_spec,
    n_cores = settings$n_cores
  )
}

extract_rda_model_settings <- function(settings, best_params) {
  list(
    seed = settings$seed,
    model_type = "rda_klar",
    features = settings$model_features,
    gamma = best_params$gamma %||% settings$gamma_fix,
    lambda = best_params$lambda %||% settings$lambda_fix,
    under_ratio = best_params$under_ratio %||% settings$under_ratio_fix,
    over_ratio = best_params$over_ratio %||% settings$over_ratio_fix
  )
}

evaluate_rda_model <- function(fitted_model, train_df, test_df) {
  train_eval <- compute_rda_predictions(fitted_model, train_df)
  test_eval <- compute_rda_predictions(fitted_model, test_df)

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics
  )
}

compute_rda_predictions <- function(fitted_model, df) {
  pred_class <- predict(fitted_model, df)$.pred_class
  pred_prob <- predict(fitted_model, df, type = "prob")

  evaluate_predictions(
    true_labels = df$metagenre,
    predicted_labels = pred_class,
    predicted_probs = pred_prob
  )
}
