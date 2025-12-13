#' Train a LightGBM classifier for metagenre prediction
#'
#' Train and evaluate a gradient boosted trees model using LightGBM.
#' Supports optional Bayesian tuning of tree depth, number of trees,
#' learning rate, mtry, min_n and sampling ratios using provided
#' resampling splits. Respects case weights stored in `case_wts`,
#' artist-based CV splits, and a reproducible seed.
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
#'   `seed`, `model_features`, `use_caseweights`, `tree_depth_fix`,
#'   `trees_fix`, `learn_rate_fix`, `mtry_fix`, `min_n_fix`,
#'   `tune_tree_depth`, `tune_trees`, `tune_learn_rate`, `tune_mtry`,
#'   `tune_min_n`, `tune_sampling`, `target_ratio_fix`,
#'   `n_cores`, `n_cores_tuning`, `initial_grid_size`,
#'   `bayes_iterations`, `uncertain_jump`.
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
#'   tree_depth_fix = 6,
#'   trees_fix = 100,
#'   learn_rate_fix = 0.1,
#'   mtry_fix = NULL,
#'   min_n_fix = 20
#' )
#' out <- train_gbm(train, test, cv_splits, settings)
#' }
#' @export
train_gbm <- function(train, test, cv_splits, settings) {
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

  any_tuning <- any_lightgbm_hyperparameter_tuned(settings)

  recipe <- create_pre_recipe(
    train,
    vars_to_remove,
    tune_sampling = settings$tune_sampling,
    target_ratio_fix = settings$target_ratio_fix,
    seed = settings$seed
  )

  model_spec <- create_lightgbm_model_spec(settings, train)

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
    message("Starting Bayesian hyperparameter tuning for LightGBM...")
    lightgbm_params <- create_lightgbm_params(workflow, train, settings)

    tune_result <- tune_bayes_workflow(
      workflow = workflow,
      train_df = train,
      cv_splits = cv_splits,
      params = lightgbm_params,
      model_type = "lightgbm",
      seed = settings$seed,
      n_cores_tuning = settings$n_cores_tuning,
      initial_grid_size = settings$initial_grid_size,
      bayes_iterations = settings$bayes_iterations,
      uncertain_jump = settings$uncertain_jump,
      grid_chunk_size = settings$grid_chunk_size,
      settings = settings,
      enable_grid_checkpoints = settings$enable_grid_checkpoints %||% TRUE,
      enable_bayes_checkpoints = settings$enable_bayes_checkpoints %||% TRUE
    )

    tuning_results <- tune_result$tuning_results
    best_params <- tune_result$best_params
    tuning_history <- extract_tuning_history(tuning_results)
    model_hash <- tune_result$model_hash
  } else {
    message("No tuning requested, fitting with fixed parameters...")
    best_params <- list(
      tree_depth = settings$tree_depth_fix,
      trees = settings$trees_fix,
      learn_rate = settings$learn_rate_fix,
      mtry = settings$mtry_fix,
      min_n = settings$min_n_fix
    )
    tuning_history <- NULL

    fixed_grid <- dplyr::tibble(
      tree_depth = settings$tree_depth_fix,
      trees = settings$trees_fix,
      learn_rate = settings$learn_rate_fix,
      mtry = settings$mtry_fix %||% NA_real_,
      min_n = settings$min_n_fix
    )
    model_hash <- compute_model_hash(
      train,
      cv_splits,
      fixed_grid,
      settings,
      "lightgbm"
    )
    message(sprintf("Model hash: %s", substr(model_hash, 1, 8)))

    log_dir <- file.path("models/classifier", "lightgbm")
    log_model_hash_info(
      model_hash,
      train,
      cv_splits,
      fixed_grid,
      settings,
      "lightgbm",
      log_dir
    )
  }

  final_fit <- finalize_and_fit_lightgbm(
    workflow,
    train,
    best_params,
    settings
  )

  message("---EVALUATING FINAL LIGHTGBM MODEL---")
  evaluation <- evaluate_lightgbm_model(final_fit, train, test)
  end_time <- Sys.time()
  evaluation$time_needed <- end_time - start_time

  if (!is.null(model_hash)) {
    cleanup_checkpoints(
      "models/classifier/checkpoints",
      "lightgbm",
      model_hash
    )
  }

  list(
    model = final_fit,
    tuning_history = tuning_history,
    evaluation = evaluation,
    model_settings = extract_lightgbm_model_settings(
      settings,
      best_params,
      model_hash
    )
  )
}

any_lightgbm_hyperparameter_tuned <- function(settings) {
  isTRUE(settings$tune_tree_depth) ||
    isTRUE(settings$tune_trees) ||
    isTRUE(settings$tune_learn_rate) ||
    isTRUE(settings$tune_mtry) ||
    isTRUE(settings$tune_min_n) ||
    isTRUE(settings$tune_sampling)
}

create_lightgbm_model_spec <- function(settings, train_df) {
  n_features <- length(settings$model_features)

  spec <- parsnip::boost_tree(
    tree_depth = settings$tree_depth_fix,
    trees = settings$trees_fix,
    learn_rate = settings$learn_rate_fix,
    mtry = settings$mtry_fix,
    min_n = settings$min_n_fix
  ) |>
    parsnip::set_engine("lightgbm", num_threads = 1) |>
    parsnip::set_mode("classification")

  spec <- add_tunable_lightgbm_params(spec, settings)
  spec
}

add_tunable_lightgbm_params <- function(spec, settings) {
  if (isTRUE(settings$tune_tree_depth)) {
    spec <- parsnip::set_args(spec, tree_depth = tune::tune())
  }
  if (isTRUE(settings$tune_trees)) {
    spec <- parsnip::set_args(spec, trees = tune::tune())
  }
  if (isTRUE(settings$tune_learn_rate)) {
    spec <- parsnip::set_args(spec, learn_rate = tune::tune())
  }
  if (isTRUE(settings$tune_mtry)) {
    spec <- parsnip::set_args(spec, mtry = tune::tune())
  }
  if (isTRUE(settings$tune_min_n)) {
    spec <- parsnip::set_args(spec, min_n = tune::tune())
  }
  spec
}

#' Create LightGBM parameter definitions for tuning
#' @param workflow Workflow to extract base params from
#' @param train_df Training data for sampling ratio bounds
#' @param settings Settings list with tune switches
#' @return dials parameter set
create_lightgbm_params <- function(workflow, train_df, settings) {
  sampling_params <- create_sampling_params(train_df)
  n_features <- length(settings$model_features)

  params <- workflow |>
    hardhat::extract_parameter_set_dials()

  if (isTRUE(settings$tune_tree_depth)) {
    params <- params |>
      update(tree_depth = dials::tree_depth(range = c(3L, 15L)))
  }

  if (isTRUE(settings$tune_trees)) {
    params <- params |>
      update(trees = dials::trees(range = c(1L, 1000L)))
  }

  if (isTRUE(settings$tune_learn_rate)) {
    params <- params |>
      update(
        learn_rate = dials::learn_rate(
          range = c(-3, -1),
          trans = scales::log10_trans()
        )
      )
  }

  if (isTRUE(settings$tune_mtry)) {
    params <- params |>
      update(mtry = dials::mtry(range = c(1L, n_features)))
  }

  if (isTRUE(settings$tune_min_n)) {
    params <- params |>
      update(min_n = dials::min_n(range = c(2L, 100L)))
  }

  if (isTRUE(settings$tune_sampling) && "target_ratio" %in% params$id) {
    params <- params |> update(target_ratio = sampling_params$target_ratio)
  }

  params
}

finalize_and_fit_lightgbm <- function(
  workflow,
  train_df,
  best_params,
  settings
) {
  best_tree_depth <- best_params$tree_depth %||% settings$tree_depth_fix
  best_trees <- best_params$trees %||% settings$trees_fix
  best_learn_rate <- best_params$learn_rate %||% settings$learn_rate_fix
  best_mtry <- best_params$mtry %||% settings$mtry_fix
  best_min_n <- best_params$min_n %||% settings$min_n_fix

  final_spec <- parsnip::boost_tree(
    tree_depth = best_tree_depth,
    trees = best_trees,
    learn_rate = best_learn_rate,
    mtry = best_mtry,
    min_n = best_min_n
  ) |>
    parsnip::set_engine("lightgbm", num_threads = settings$n_cores) |>
    parsnip::set_mode("classification")

  finalize_and_fit(
    workflow,
    train_df,
    best_params,
    final_spec = final_spec,
    n_cores = settings$n_cores
  )
}

extract_lightgbm_model_settings <- function(
  settings,
  best_params,
  model_hash = NULL
) {
  list(
    seed = settings$seed,
    model_type = "lightgbm",
    features = settings$model_features,
    tree_depth = best_params$tree_depth %||% settings$tree_depth_fix,
    trees = best_params$trees %||% settings$trees_fix,
    learn_rate = best_params$learn_rate %||% settings$learn_rate_fix,
    mtry = best_params$mtry %||% settings$mtry_fix,
    min_n = best_params$min_n %||% settings$min_n_fix,
    target_ratio = best_params$target_ratio %||% settings$target_ratio_fix,
    model_hash = model_hash
  )
}

evaluate_lightgbm_model <- function(fitted_model, train_df, test_df) {
  train_eval <- compute_lightgbm_predictions(fitted_model, train_df)
  test_eval <- compute_lightgbm_predictions(fitted_model, test_df)

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics
  )
}

compute_lightgbm_predictions <- function(fitted_model, df) {
  pred_class <- predict(fitted_model, df)$.pred_class
  pred_prob <- predict(fitted_model, df, type = "prob")

  evaluate_predictions(
    true_labels = df$metagenre,
    predicted_labels = pred_class,
    predicted_probs = pred_prob
  )
}
