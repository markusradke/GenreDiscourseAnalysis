#' Train and evaluate random-forest models using tidymodels
#'
#' @param train Training data frame
#' @param test Test data frame
#' @param cv_splits Cross-validation splits (required for tuning)
#' @param settings Named list with tuning switches (\code{tune_mtry}, \code{tune_min_n},
#'   \code{tune_max_depth}, \code{tune_downsample}, \code{tune_upsample}), fixed values
#'   (\code{mtry_fix}, \code{min.node.size_fix}, \code{max.depth_fix}), and other params.
#' @return List with \code{model}, \code{model_settings}, \code{evaluation},
#'   and optionally \code{tuning_history}.
#' @export
train_random_forest <- function(train, test, cv_splits, settings) {
  start_time <- Sys.time()
  if (
    is.null(settings$model_features) ||
      !is.character(settings$model_features)
  ) {
    stop("Please provide settings$model_features as character vector.")
  }

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
  set.seed(settings$seed)

  any_tuning <- any_hyperparameter_tuned(settings)

  rf_workflow <- create_rf_workflow(
    train_df = train,
    vars_to_remove = vars_to_remove,
    settings = settings
  )

  tuning_fit_result <- if (any_tuning && !is.null(cv_splits)) {
    tune_and_fit_workflow(
      rf_workflow,
      train,
      cv_splits,
      settings,
      ntrees_final = settings$ntrees
    )
  } else {
    fit_workflow(rf_workflow, train, settings$n_cores)
  }

  fitted_workflow <- tuning_fit_result$fitted_workflow
  rf_model <- extract_ranger_model(fitted_workflow)
  model_settings <- extract_model_settings(
    rf_model,
    tuning_fit_result$tuning_results,
    settings$under_ratio_fix,
    settings$over_ratio_fix
  )

  evaluation <- evaluate_with_metrics(
    fitted_workflow,
    train,
    test,
    varimp_top_n = settings$varimp_top_n
  )

  end_time <- Sys.time()
  time_needed <- end_time - start_time
  message(paste(
    "Time needed for training and evaluating model:",
    format(time_needed, units = "auto")
  ))
  evaluation$time_needed <- time_needed

  tuning_history <- if (!is.null(tuning_fit_result$tuning_results)) {
    extract_tuning_history(tuning_fit_result$tuning_results)
  } else {
    NULL
  }

  list(
    model = fitted_workflow,
    model_settings = model_settings,
    evaluation = evaluation,
    tuning_history = tuning_history
  )
}

create_rf_workflow <- function(train_df, vars_to_remove, settings) {
  any_tuning <- any_hyperparameter_tuned(settings)

  rf_recipe <- create_pre_recipe(
    train_df,
    vars_to_remove,
    tune_downsample = settings$tune_downsample,
    tune_upsample = settings$tune_upsample,
    under_ratio_fix = settings$under_ratio_fix,
    over_ratio_fix = settings$over_ratio_fix,
    seed = settings$seed
  )

  rf_spec <- create_rf_model_spec(train_df, settings, any_tuning)

  workflow <- workflows::workflow() |>
    workflows::add_recipe(rf_recipe) |>
    workflows::add_model(rf_spec)

  if (isTRUE(settings$use_caseweights)) {
    workflow <- workflow |>
      workflows::add_case_weights(case_wts)
  }
  print(workflow)
  workflow
}

create_rf_model_spec <- function(train_df, settings, any_tuning) {
  class_weights <- get_class_weights(train_df$metagenre)
  workers <- settings$n_cores_tuning
  n_threads <- get_number_of_threads_per_worker(settings$n_cores, workers)

  rf_spec <- parsnip::rand_forest(
    trees = if (any_tuning) settings$ntrees_tuning else settings$ntrees,
    mtry = settings$mtry_fix,
    min_n = settings$min.node.size_fix
  ) |>
    parsnip::set_engine(
      "ranger",
      importance = settings$importance,
      probability = TRUE,
      max.depth = settings$max.depth_fix,
      class.weights = class_weights,
      num.threads = n_threads,
      verbose = TRUE,
      keep.inbag = TRUE
    ) |>
    parsnip::set_mode("classification")

  rf_spec <- add_tunable_rf_params(rf_spec, settings)
  rf_spec
}

add_tunable_rf_params <- function(spec, settings) {
  if (isTRUE(settings$tune_mtry)) {
    spec <- parsnip::set_args(spec, mtry = tune::tune())
  }
  if (isTRUE(settings$tune_min_n)) {
    spec <- parsnip::set_args(spec, min_n = tune::tune())
  }
  if (isTRUE(settings$tune_max_depth)) {
    spec <- parsnip::set_args(spec, max.depth = tune::tune())
  }
  spec
}

any_hyperparameter_tuned <- function(settings) {
  isTRUE(settings$tune_mtry) ||
    isTRUE(settings$tune_min_n) ||
    isTRUE(settings$tune_max_depth) ||
    isTRUE(settings$tune_downsample) ||
    isTRUE(settings$tune_upsample)
}

tune_and_fit_workflow <- function(
  workflow,
  train_df,
  cv_splits,
  settings,
  ntrees_final
) {
  rf_params <- create_rf_params(workflow, train_df, settings)
  tune_result <- tune_bayes_workflow(
    workflow = workflow,
    train_df = train_df,
    cv_splits = cv_splits,
    params = rf_params,
    model_type = "rf",
    seed = settings$seed,
    n_cores_tuning = settings$n_cores_tuning,
    initial_grid_size = settings$initial_grid_size,
    bayes_iterations = settings$bayes_iterations,
    uncertain_jump = settings$uncertain_jump
  )

  best_params <- tune_result$best_params
  class_weights <- get_class_weights(train_df$metagenre)

  best_max_depth <- if (isTRUE(settings$tune_max_depth)) {
    best_params$max.depth
  } else {
    settings$max.depth_fix
  }

  final_spec <- parsnip::rand_forest(
    trees = ntrees_final,
    mtry = best_params$mtry %||% settings$mtry_fix,
    min_n = best_params$min_n %||% settings$min.node.size_fix
  ) |>
    parsnip::set_engine(
      "ranger",
      importance = "permutation",
      probability = TRUE,
      max.depth = best_max_depth,
      class.weights = class_weights,
      verbose = TRUE,
      keep.inbag = TRUE,
      num.threads = settings$n_cores
    ) |>
    parsnip::set_mode("classification")

  fitted_workflow <- finalize_and_fit(
    workflow,
    train_df,
    best_params,
    final_spec = final_spec,
    n_cores = settings$n_cores
  )

  list(
    fitted_workflow = fitted_workflow,
    tuning_results = tune_result$tuning_results
  )
}

#' Create RF parameter definitions for tuning
#' @param workflow Workflow to extract base params from
#' @param train_df Training data
#' @param settings Settings list with tune switches
#' @return dials parameter set
create_rf_params <- function(workflow, train_df, settings) {
  sampling_params <- create_sampling_params(train_df)
  n_predictors <- ncol(train_df) - 4

  params <- workflow |>
    hardhat::extract_parameter_set_dials()

  if (isTRUE(settings$tune_downsample)) {
    params <- params |> update(under_ratio = sampling_params$under_ratio)
  }

  if (isTRUE(settings$tune_upsample)) {
    params <- params |> update(over_ratio = sampling_params$over_ratio)
  }

  if (isTRUE(settings$tune_mtry)) {
    params <- params |> update(mtry = dials::mtry(range = c(1, n_predictors)))
  }

  if (isTRUE(settings$tune_min_n)) {
    params <- params |> update(min_n = dials::min_n(range = c(1, 100)))
  }

  if (isTRUE(settings$tune_max_depth)) {
    max_depth_param <- dials::new_quant_param(
      type = "integer",
      range = c(1L, 100L),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(max.depth = "Max Tree Depth")
    )
    params <- params |> update(max.depth = max_depth_param)
  }

  params
}

fit_workflow <- function(workflow, train_df, n_cores) {
  print_phase_info("FINAL MODEL FIT (no tuning)", n_cores, is_final_fit = TRUE)
  future::plan(future::multisession, workers = n_cores)
  fitted_workflow <- parsnip::fit(workflow, data = train_df)
  future::plan(future::sequential)
  list(fitted_workflow = fitted_workflow, tuning_results = NULL)
}


get_class_weights <- function(metagenres) {
  tbl <- table(metagenres)
  total <- sum(tbl)
  n_classes <- length(tbl)
  class_weights <- sqrt(total / (n_classes * tbl)) # take square root to reduce extreme weights
  class_weights
}


extract_ranger_model <- function(fitted_workflow) {
  fitted_workflow |>
    workflows::extract_fit_parsnip() |>
    purrr::pluck("fit")
}

extract_model_settings <- function(
  ranger_model,
  tuning_results,
  under_ratio_fix,
  over_ratio_fix = 0.5
) {
  if (!is.null(tuning_results)) {
    bayes_iterations <- max(tuning_results$.iter)
    best_params <- tune::select_best(
      tuning_results,
      metric = "macro_f1_with_zeros"
    )
    best_under_ratio <- best_params$under_ratio
    best_over_ratio <- best_params$over_ratio
  } else {
    bayes_iterations <- NULL
    best_under_ratio <- under_ratio_fix
    best_over_ratio <- over_ratio_fix
  }

  list(
    ntrees = ranger_model$num.trees,
    mtry = ranger_model$mtry,
    under_ratio = best_under_ratio,
    over_ratio = best_over_ratio,
    bayes_iterations = bayes_iterations,
    max.depth = ranger_model$max.depth,
    min_n = ranger_model$min.node.size,
    nindependent = ranger_model$num.independent.variables,
    vip.mode = ranger_model$importance.mode,
    splitrule = ranger_model$splitrule,
    treetype = ranger_model$treetype
  )
}

evaluate_with_metrics <- function(
  fitted_workflow,
  train_df,
  test_df,
  varimp_top_n = 40
) {
  train_eval <- compute_workflow_predictions(fitted_workflow, train_df)
  test_eval <- compute_workflow_predictions(fitted_workflow, test_df)

  varimp_df <- extract_variable_importance(fitted_workflow)

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics,
    varimp = varimp_df
  )
}

compute_workflow_predictions <- function(fitted_workflow, df) {
  pred_class <- predict(fitted_workflow, new_data = df, type = "class")
  pred_prob <- predict(fitted_workflow, new_data = df, type = "prob")

  evaluate_predictions(
    true_labels = df$metagenre,
    predicted_labels = pred_class$.pred_class,
    predicted_probs = pred_prob
  )
}

extract_variable_importance <- function(fitted_workflow) {
  ranger_model <- extract_ranger_model(fitted_workflow)
  varimp <- ranger::importance(ranger_model)

  data.frame(Variable = names(varimp), Importance = varimp) |>
    dplyr::arrange(dplyr::desc(Importance))
}

get_number_of_threads_per_worker <- function(total_threads, n_workers) {
  threads_per_worker <- floor(total_threads / n_workers)
  if (threads_per_worker < 1) {
    threads_per_worker <- 1
  }
  as.integer(threads_per_worker)
}
