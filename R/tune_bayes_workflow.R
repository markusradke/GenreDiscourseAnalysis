# Shared Bayesian tuning infrastructure for all classification models

print_tuning_core_guidance <- function(n_folds, initial_grid_size) {
  total_tasks <- n_folds * initial_grid_size

  message("--- Parallel Tuning Configuration ---")
  message("\nCore selection guidance:\n")
  message(sprintf(
    "  - Initial grid: %d tasks (%d folds x %d grid points)",
    total_tasks,
    n_folds,
    initial_grid_size
  ))
  message(sprintf(
    "  - Bayesian phase: %d tasks per iteration (1 per fold)",
    n_folds
  ))
  message("  - Use n_cores_tuning as multiple of n_folds to avoid idle workers")
  message(
    "  - For GLMNET: Try to use as many workers (n_cores_tuning) as possible (1 thread per worker)"
  )
  message(
    "  - For random forests: Automatically uses threads_per_worker for hybrid parallelism"
  )
  message(
    "    Threads per worker = max cores / n_cores_tuning |> floor()"
  )
  message(
    "    It is recommended to set n_cores_tuning = n_folds to maximize resource usage during bayesian phase"
  )
  message("  - Monitor worker memory with htop (RES column)")
  message("  - RAM needed: ~3GB main + n_workers x per_worker_memory")
  message("--------------------------------------")
}

print_phase_info <- function(
  phase,
  n_workers,
  threads_per_worker = 1,
  is_final_fit = FALSE
) {
  if (is_final_fit) {
    message(sprintf("--- %s: %d threads ---", phase, n_workers))
  } else {
    total_cores <- n_workers * threads_per_worker
    if (threads_per_worker > 1) {
      message(sprintf(
        "--- %s: %d workers x %d threads = %d cores ---",
        phase,
        n_workers,
        threads_per_worker,
        total_cores
      ))
    } else {
      message(sprintf("--- %s: %d workers ---", phase, n_workers))
    }
  }
}

#' Create standard metric set for classification tuning
#' @return A yardstick metric set
create_tuning_metrics <- function() {
  yardstick::metric_set(
    macro_f1_with_zeros,
    yardstick::accuracy,
    yardstick::kap,
    yardstick::mcc
  )
}

#' Create sampling ratio parameter definitions
#' @param train_df Training data frame with metagenre column
#' @return Named list with under_ratio and over_ratio dials params
create_sampling_params <- function(train_df) {
  max_imbalance <- max(table(train_df$metagenre)) /
    min(table(train_df$metagenre))

  list(
    under_ratio = dials::new_quant_param(
      type = "double",
      range = c(1, max_imbalance),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(under_ratio = "Under-Sampling Ratio")
    ),
    over_ratio = dials::new_quant_param(
      type = "double",
      range = c(0.1, 1.0),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(over_ratio = "SMOTENC over_ratio")
    )
  )
}

#' Run Bayesian hyperparameter tuning on a workflow
#'
#' @param workflow A tidymodels workflow with tune() placeholders
#' @param train_df Training data frame
#' @param cv_splits Cross-validation splits
#' @param params dials parameter set (from extract_parameter_set_dials)
#' @param seed Random seed
#' @param n_cores_tuning Number of parallel workers for tuning
#' @param initial_grid_size Number of points for initial space-filling grid
#' @param bayes_iterations Number of Bayesian optimization iterations
#' @param uncertain_jump Uncertainty parameter for tune_bayes
#' @return List with tuning_results, best_params
tune_bayes_workflow <- function(
  workflow,
  train_df,
  cv_splits,
  params,
  seed = 42,
  n_cores_tuning = 4,
  initial_grid_size = 10,
  bayes_iterations = 15,
  uncertain_jump = 5
) {
  n_folds <- length(cv_splits$splits)
  print_tuning_core_guidance(n_folds, initial_grid_size)
  metric_set <- create_tuning_metrics()

  gridcontrol <- tune::control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything"
  )
  bayescontrol <- tune::control_bayes(
    verbose = TRUE,
    verbose_iter = TRUE,
    no_improve = 10,
    seed = seed,
    uncertain = uncertain_jump,
    parallel_over = "everything"
  )

  initial_grid <- dials::grid_space_filling(
    params,
    size = initial_grid_size,
    type = "latin_hypercube"
  )
  message(sprintf("Initial grid (%d models):", nrow(initial_grid)))
  print(initial_grid, n = nrow(initial_grid))

  options(future.globals.maxSize = 15L * 1024^3)
  future::plan(future::multisession, workers = n_cores_tuning)
  on.exit({
    future::plan(future::sequential)
    options(future.globals.maxSize = 500L * 1024^2)
  })

  set.seed(seed)
  print_phase_info("INITIAL GRID TUNING", n_cores_tuning)
  initial_results <- tune::tune_grid(
    workflow,
    resamples = cv_splits,
    grid = initial_grid,
    metrics = metric_set,
    control = gridcontrol
  )

  print_phase_info("BAYESIAN OPTIMIZATION", n_cores_tuning)
  tuning_results <- tune::tune_bayes(
    workflow,
    resamples = cv_splits,
    metrics = metric_set,
    initial = initial_results,
    param_info = params,
    iter = bayes_iterations,
    control = bayescontrol #,
    # objective = "macro_f1_with_zeros"
  )

  best_params <- tune::select_best(
    tuning_results,
    metric = "macro_f1_with_zeros"
  )
  message(
    "Best hyperparameters: ",
    paste(names(best_params), "=", unlist(best_params), collapse = ", ")
  )

  list(
    tuning_results = tuning_results,
    best_params = best_params
  )
}

#' Finalize and fit a tuned workflow
#'
#' @param workflow Original workflow with tune() placeholders
#' @param train_df Training data
#' @param best_params Best parameters from tuning
#' @param final_spec Optional: replacement model spec for final fit
#' @param n_cores Number of cores/threads for final fit
#' @return Fitted workflow
finalize_and_fit <- function(
  workflow,
  train_df,
  best_params,
  final_spec = NULL,
  n_cores = 1L
) {
  final_workflow <- tune::finalize_workflow(workflow, best_params)

  if (!is.null(final_spec)) {
    final_workflow <- final_workflow |>
      workflows::update_model(final_spec)
  }
  print_phase_info("FINAL MODEL FIT", n_cores, is_final_fit = TRUE)
  parsnip::fit(final_workflow, data = train_df)
}
