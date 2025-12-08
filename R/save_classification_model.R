#' Save a classification model with minimal footprint
#'
#' Saves all components needed for prediction, evaluation, and documentation
#' while removing unnecessary data that bloats file size.
#'
#' @param model_result List returned by train_glmnet or train_random_forest
#' @param name Model name for file prefix
#' @param subfolder Subfolder within models/classifier/
#' @param train_df Training data (for metadata extraction only)
#' @param test_df Test data (for metadata extraction only)
#' @return Invisibly returns the path to the saved model bundle
#' @export
save_classification_model <- function(
  model_result,
  name,
  subfolder,
  train_df = NULL,
  test_df = NULL
) {
  base_path <- sprintf("models/classifier/%s/%s", subfolder, name)
  message(sprintf("Saving classification model bundle: %s...", name))

  minimal_model <- butcher_workflow(model_result$model)
  saveRDS(minimal_model, paste0(base_path, "_model.rds"))
  original_size <- as.numeric(object.size(model_result$model)) / 1024^2
  new_size <- as.numeric(object.size(minimal_model)) / 1024^2

  message(sprintf(
    "  Model size: %.2f MB -> %.2f MB (%.0f%% reduction)",
    original_size,
    new_size,
    (1 - new_size / original_size) * 100
  ))

  saveRDS(model_result$evaluation, paste0(base_path, "_evaluation.rds"))

  enriched_settings <- enrich_model_settings(
    model_result$model_settings,
    model_result$model,
    train_df,
    test_df
  )
  saveRDS(enriched_settings, paste0(base_path, "_settings.rds"))

  saveRDS(
    model_result$tuning_history,
    paste0(base_path, "_tuning_history.rds")
  )

  invisible(base_path)
}

#' Remove training data from workflow while keeping prediction capability
#' @param fitted_workflow A fitted tidymodels workflow
#' @return Butchered workflow with minimal memory footprint
butcher_workflow <- function(fitted_workflow) {
  if (!inherits(fitted_workflow, "workflow")) {
    return(fitted_workflow)
  }

  butchered <- butcher::butcher(fitted_workflow)

  # Additional cleanup: remove mold data (predictors/outcomes used for fitting)
  # The mold stores a copy of the training data which can be huge
  if (!is.null(butchered$pre$mold)) {
    mold <- butchered$pre$mold

    if (!is.null(mold$predictors)) {
      predictor_info <- list(
        names = colnames(mold$predictors),
        nrow = nrow(mold$predictors),
        types = vapply(mold$predictors, function(x) class(x)[1], character(1))
      )
      butchered$pre$mold$predictors <- NULL
      attr(butchered$pre$mold, "predictor_info") <- predictor_info
    }

    if (!is.null(mold$outcomes)) {
      outcome_info <- list(
        names = colnames(mold$outcomes),
        nrow = nrow(mold$outcomes),
        levels = if (is.factor(mold$outcomes[[1]])) {
          levels(mold$outcomes[[1]])
        } else {
          NULL
        }
      )
      butchered$pre$mold$outcomes <- NULL
      attr(butchered$pre$mold, "outcome_info") <- outcome_info
    }

    if (!is.null(mold$extras)) {
      butchered$pre$mold$extras <- NULL
    }
  }

  butchered
}

#' Enrich model settings with workflow and data metadata
#' @param settings Original model settings
#' @param fitted_workflow Fitted workflow
#' @param train_df Training data (optional)
#' @param test_df Test data (optional)
#' @return Enriched settings list
enrich_model_settings <- function(
  settings,
  fitted_workflow,
  train_df = NULL,
  test_df = NULL
) {
  enriched <- settings

  enriched$workflow_info <- extract_workflow_info(fitted_workflow)

  if (!is.null(train_df)) {
    enriched$train_metadata <- list(
      n_rows = nrow(train_df),
      n_cols = ncol(train_df),
      feature_names = setdiff(
        colnames(train_df),
        c("metagenre", "case_wts", "track.s.id", "artist.s.id")
      ),
      n_artists = length(unique(train_df$artist.s.id)),
      outcome_distribution = as.list(table(train_df$metagenre))
    )
  }

  if (!is.null(test_df)) {
    enriched$test_metadata <- list(
      n_rows = nrow(test_df),
      n_cols = ncol(test_df),
      n_artists = length(unique(test_df$artist.s.id)),
      outcome_distribution = as.list(table(test_df$metagenre))
    )
  }

  enriched$saved_at <- Sys.time()

  enriched
}

#' Extract workflow preprocessing steps as documentation
#' @param fitted_workflow Fitted workflow
#' @return List with recipe steps and model spec info
extract_workflow_info <- function(fitted_workflow) {
  if (!inherits(fitted_workflow, "workflow")) {
    return(list(type = "non-workflow"))
  }

  info <- list()

  recipe <- tryCatch(
    workflows::extract_recipe(fitted_workflow),
    error = function(e) NULL
  )

  if (!is.null(recipe)) {
    info$recipe_steps <- purrr::map(recipe$steps, function(step) {
      list(
        type = class(step)[1],
        id = step$id,
        trained = step$trained,
        skip = step$skip,
        role = ifelse(!is.null(step$role), step$role, NA)
      )
    })
  }

  spec <- tryCatch(
    workflows::extract_spec_parsnip(fitted_workflow),
    error = function(e) NULL
  )

  if (!is.null(spec)) {
    info$model_spec <- list(
      mode = spec$mode,
      engine = spec$engine,
      args = lapply(spec$args, function(x) {
        if (rlang::is_quosure(x)) rlang::quo_text(x) else as.character(x)
      })
    )
  }

  info
}

#' Extract compact tuning history with all metrics
#' @param tuning_results tune_results object from tune_grid or tune_bayes
#' @return Data frame with iteration, parameters, and all metrics (mean + std)
extract_tuning_history <- function(tuning_results) {
  metrics_df <- tune::collect_metrics(tuning_results, summarize = TRUE)

  params_cols <- setdiff(
    colnames(metrics_df),
    c(".metric", ".estimator", "mean", "std_err", "n", ".config", ".iter")
  )

  wide_df <- metrics_df |>
    dplyr::select(
      dplyr::all_of(c(
        params_cols,
        ".iter",
        ".config",
        ".metric",
        "mean",
        "std_err"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = .metric,
      values_from = c(mean, std_err),
      names_glue = "{.metric}_{.value}"
    )

  if (".iter" %in% colnames(wide_df)) {
    wide_df <- wide_df |> dplyr::arrange(.iter)
  }

  wide_df
}

#' Load a saved classification model bundle
#' @param name Model name
#' @param subfolder Subfolder within models/classifier/
#' @return List with model, evaluation, settings, and tuning_history
load_classification_model <- function(folder, name) {
  base_path <- sprintf("%s/%s", folder, name)

  result <- list(
    model = readRDS(paste0(base_path, "_model.rds")),
    evaluation = readRDS(paste0(base_path, "_evaluation.rds")),
    settings = readRDS(paste0(base_path, "_settings.rds"))
  )

  tuning_path <- paste0(base_path, "_tuning_history.rds")
  if (file.exists(tuning_path)) {
    result$tuning_history <- readRDS(tuning_path)
  }

  result
}
