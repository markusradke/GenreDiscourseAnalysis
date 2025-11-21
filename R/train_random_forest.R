#' Train and evaluate random-forest models using tidymodels
#'
#' @param settings Named list containing \code{features_after_impute}, \code{under_ratio},
#'   \code{varimp_top_n}, \code{seed}, \code{run_rf_low}, \code{run_rf_high}, \code{tune_hyperparameters},
#'   \code{n_cores}, and optionally \code{ntrees}, \code{mtry}, \code{min.node.size},
#'   \code{importance}, \code{initial_grid_density}, \code{bayes_iterations}, \code{uncertain_jump}.
#' @param datasets List with \code{low} and \code{high} elements containing \code{train}, \code{test},
#'   and optionally \code{cv_splits}.
#' @return List with \code{low} and \code{high} results containing \code{model}, \code{model_settings},
#'   \code{evaluation}, \code{train_df}, \code{test_df}, and optionally \code{tuning_results}.
#' @export
train_and_evaluate_rf_tidy <- function(settings, datasets) {
  start_time <- Sys.time()
  if (
    is.null(settings$features_after_impute) ||
      !is.character(settings$features_after_impute)
  ) {
    stop("Please provide settings$features_after_impute as character vector.")
  }

  feats <- if ("metagenre" %in% settings$features_after_impute) {
    settings$features_after_impute
  } else {
    c("metagenre", settings$features_after_impute)
  }

  results <- list()

  for (detail in c("low", "high")) {
    if (!isTRUE(settings[[paste0("run_rf_", detail)]])) {
      results[[detail]] <- NULL
      next
    }

    # Select features - case_wts column will be automatically preserved by tidymodels
    train_df <- datasets[[detail]]$train |> dplyr::select(dplyr::all_of(feats))
    test_df <- datasets[[detail]]$test |> dplyr::select(dplyr::all_of(feats))

    if ("case_wts" %in% names(datasets[[detail]]$train)) {
      train_df$case_wts <- datasets[[detail]]$train$case_wts
      test_df$case_wts <- datasets[[detail]]$test$case_wts
      message(
        "Case weights detected and will be used during training and tuning"
      )
    }

    cv_splits <- datasets[[detail]]$cv_splits

    set.seed(settings$seed)
    message(sprintf("---TRAINING MODEL FOR %s---", toupper(detail)))

    rf_workflow <- create_rf_workflow(
      train_df = train_df,
      tune_hyperparameters = settings$tune_hyperparameters,
      under_ratio = settings$under_ratio,
      ntrees = settings$ntrees,
      ntrees_tuning = settings$ntrees_tuning,
      mtry = settings$mtry,
      min.node.size = settings$min.node.size,
      importance = settings$importance,
      seed = settings$seed
    )

    tuning_fit_result <- if (
      settings$tune_hyperparameters && !is.null(cv_splits)
    ) {
      tune_and_fit_workflow(
        rf_workflow,
        train_df,
        cv_splits,
        settings$seed,
        settings$n_cores,
        settings$initial_grid_density,
        settings$bayes_iterations,
        settings$uncertain_jump,
        ntrees_final = settings$ntrees
      )
    } else {
      fit_workflow(rf_workflow, train_df, settings$n_cores)
    }

    fitted_workflow <- tuning_fit_result$fitted_workflow
    rf_model <- extract_ranger_model(fitted_workflow)
    model_settings <- extract_model_settings(
      rf_model,
      tuning_fit_result$tuning_results
    )

    evaluation <- evaluate_with_metrics(
      fitted_workflow,
      train_df,
      test_df,
      varimp_top_n = settings$varimp_top_n
    )

    end_time <- Sys.time()
    time_needed <- end_time - start_time
    message(paste(
      "Time needed for training and evaluating model:",
      time_needed
    ))
    evaluation$time_needed <- time_needed

    results[[detail]] <- list(
      model = rf_model,
      model_settings = model_settings,
      evaluation = evaluation,
      train_df = train_df,
      test_df = test_df,
      tuning_results = tuning_fit_result$tuning_results
    )
  }

  results
}

create_rf_workflow <- function(
  train_df,
  tune_hyperparameters = FALSE,
  under_ratio = 2,
  ntrees = 1000,
  ntrees_tuning = 500,
  mtry = NULL,
  min.node.size = NULL,
  importance = "impurity",
  seed = 42
) {
  rf_recipe <- recipes::recipe(metagenre ~ ., data = train_df) |>
    themis::step_downsample(
      metagenre,
      under_ratio = if (tune_hyperparameters) tune::tune() else under_ratio,
      seed = seed,
      skip = TRUE
    ) |>
    recipes::step_nzv(recipes::all_numeric_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  trees_value <- if (tune_hyperparameters) ntrees_tuning else ntrees
  class_weights <- get_class_weights(train_df$metagenre)

  # Note: case.weights will be automatically extracted from case_wts column
  # by tidymodels during both CV tuning and final fit
  rf_spec <- parsnip::rand_forest(
    trees = trees_value,
    mtry = if (tune_hyperparameters) tune::tune() else mtry,
    min_n = if (tune_hyperparameters) tune::tune() else min.node.size
  ) |>
    parsnip::set_engine(
      "ranger",
      importance = importance,
      probability = TRUE,
      class.weights = class_weights,
      verbose = TRUE
    ) |>
    parsnip::set_mode("classification")

  workflow <- workflows::workflow() |>
    workflows::add_recipe(rf_recipe) |>
    workflows::add_model(rf_spec)
  print(workflow)
  workflow
}

tune_and_fit_workflow <- function(
  workflow,
  train_df,
  cv_splits,
  seed,
  n_cores,
  initial_grid_density,
  bayes_iterations,
  uncertain_jump,
  ntrees_final
) {
  gridcontrol <- tune::control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything"
  )
  bayescontrol <- tune::control_bayes(
    verbose = TRUE,
    seed = seed,
    uncertain = uncertain_jump,
    parallel_over = "everything"
  )

  metric_set <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    macro_f1_with_zeros,
    yardstick::mcc,
    yardstick::mn_log_loss
  )

  n_predictors <- ncol(train_df) - 2 # metagenre and case_wts
  rf_params <- workflow |>
    hardhat::extract_parameter_set_dials() |>
    update(
      under_ratio = dials::new_quant_param(
        type = "double",
        range = c(
          1,
          # look through whole parameter space
          max(table(train_df$metagenre)) / min(table(train_df$metagenre))
        ),
        inclusive = c(TRUE, TRUE),
        trans = NULL,
        label = c(under_ratio = "Under-Sampling Ratio")
      ),
      mtry = dials::mtry(range = c(1, n_predictors)),
      min_n = dials::min_n(range = c(1, 100))
    )

  initial_grid <- dials::grid_regular(
    rf_params,
    levels = initial_grid_density
  )

  set.seed(seed)
  # future::plan(future::multisession, workers = n_cores)
  message("---TUNE INITIAL GRID---")
  print(initial_grid)
  initial_results <- tune::tune_grid(
    workflow,
    resamples = cv_splits,
    grid = initial_grid,
    metrics = metric_set,
    control = gridcontrol
  )

  message("#--TUNE BAYESIAN OPTIMIZATION---")
  tuning_results <- tune::tune_bayes(
    workflow,
    resamples = cv_splits,
    metrics = metric_set,
    initial = initial_results,
    param_info = rf_params,
    iter = bayes_iterations,
    control = bayescontrol
  )
  best_params <- tune::select_best(
    tuning_results,
    metric = "macro_f1_with_zeros"
  )
  message(
    "Best hyperparameters: ",
    paste(names(best_params), "=", unlist(best_params), collapse = ", ")
  )

  class_weights <- get_class_weights(train_df$metagenre)

  # future::plan(future::multisession, workers = n_cores)
  # Case weights will be automatically extracted from case_wts column in train_df
  final_spec <- parsnip::rand_forest(
    trees = ntrees_final,
    mtry = best_params$mtry,
    min_n = best_params$min_n
  ) |>
    parsnip::set_engine(
      "ranger",
      importance = "permutation",
      probability = TRUE,
      class.weights = class_weights,
      verbose = TRUE
    ) |>
    parsnip::set_mode("classification")

  message("---TUNE FINAL MODEL---")
  final_workflow <- workflow |>
    tune::finalize_workflow(best_params) |>
    workflows::update_model(final_spec)

  fitted_workflow <- parsnip::fit(final_workflow, data = train_df)
  # future::plan(future::sequential)

  list(
    fitted_workflow = fitted_workflow,
    tuning_results = tuning_results
  )
}

fit_workflow <- function(workflow, train_df, n_cores) {
  # Case weights are automatically extracted from case_wts column if present
  future::plan(future::multisession, workers = n_cores)
  fitted_workflow <- parsnip::fit(workflow, data = train_df)
  future::plan(future::sequential)
  list(fitted_workflow = fitted_workflow, tuning_results = NULL)
}

extract_ranger_model <- function(fitted_workflow) {
  fitted_workflow |>
    workflows::extract_fit_parsnip() |>
    purrr::pluck("fit")
}

extract_model_settings <- function(ranger_model, tuning_results) {
  bayes_iterations <- max(tuning_results$.iter)
  best_under_ratio <- tune::select_best(
    tuning_results,
    metric = "f_meas"
  )$under_ratio

  list(
    ntrees = ranger_model$num.trees,
    mtry = ranger_model$mtry,
    under_ratio = best_under_ratio,
    bayes_iterations = bayes_iterations,
    max.depth = ranger_model$max.depth,
    min.node.size = ranger_model$min.node.size,
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
  train_eval <- compute_confusion_and_metrics(fitted_workflow, train_df)
  test_eval <- compute_confusion_and_metrics(fitted_workflow, test_df)

  ranger_model <- extract_ranger_model(fitted_workflow)
  varimp <- ranger::importance(ranger_model)
  varimp_df <- data.frame(Variable = names(varimp), Importance = varimp) |>
    dplyr::arrange(dplyr::desc(Importance))

  list(
    confusion_train = train_eval$cm,
    metrics_train = train_eval$metrics,
    confusion_test = test_eval$cm,
    metrics_test = test_eval$metrics,
    varimp = varimp_df
  )
}

compute_confusion_and_metrics <- function(fitted_workflow, df) {
  predictions_class <- predict(fitted_workflow, new_data = df, type = "class")
  predictions_prob <- predict(fitted_workflow, new_data = df, type = "prob")

  predictions_df <- dplyr::bind_cols(
    df |> dplyr::select(metagenre),
    predictions_class,
    predictions_prob
  )

  # Ensure all truth levels have corresponding probability columns
  truth_levels <- levels(df$metagenre)
  pred_cols <- grep("^\\.pred_", names(predictions_df), value = TRUE)
  pred_cols <- pred_cols[pred_cols != ".pred_class"]
  pred_classes <- gsub("^\\.pred_", "", pred_cols)

  missing_classes <- setdiff(truth_levels, pred_classes)

  if (length(missing_classes) > 0) {
    message(
      sprintf(
        "Adding zero probability columns for %d missing classes: %s",
        length(missing_classes),
        paste(head(missing_classes, 3), collapse = ", ")
      )
    )

    # Add columns with zero probability for missing classes
    for (missing_class in missing_classes) {
      col_name <- paste0(".pred_", missing_class)
      predictions_df[[col_name]] <- 0
    }
  }

  all_levels <- union(
    levels(df$metagenre),
    levels(predictions_class$.pred_class)
  )

  conf_table <- table(
    Actual = factor(df$metagenre, levels = all_levels),
    Predicted = factor(predictions_class$.pred_class, levels = all_levels)
  )

  cm_df <- as.data.frame(conf_table) |>
    dplyr::group_by(Actual) |>
    dplyr::mutate(
      relfreq = Freq / sum(Freq),
      labelcolor = relfreq > 0.5
    ) |>
    dplyr::ungroup()

  metrics <- calculate_yardstick_metrics(predictions_df)

  list(cm = cm_df, metrics = metrics)
}

calculate_yardstick_metrics <- function(predictions_df) {
  metric_set <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    macro_f1_with_zeros,
    yardstick::mcc,
    yardstick::mn_log_loss
  )

  metrics_result <- metric_set(
    predictions_df,
    truth = metagenre,
    estimate = .pred_class,
    dplyr::matches(".pred_(?!class$)", perl = TRUE)
  )

  metrics_list <- setNames(
    as.list(metrics_result$.estimate),
    metrics_result$.metric
  )

  list(
    accuracy = metrics_list$accuracy,
    kappa = metrics_list$kap,
    f1macro = metrics_list$macro_f1_zeros,
    mcc = metrics_list$mcc,
    mn_log_loss = metrics_list$mn_log_loss
  )
}

get_class_weights <- function(metagenres) {
  tbl <- table(metagenres)
  total <- sum(tbl)
  n_classes <- length(tbl)
  class_weights <- sqrt(total / (n_classes * tbl)) # take square root to reduce extreme weights
  class_weights
}
