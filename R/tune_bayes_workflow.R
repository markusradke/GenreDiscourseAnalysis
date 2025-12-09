# Shared Bayesian tuning infrastructure for all classification models

tune_grid_chunked <- function(
  workflow,
  cv_splits,
  grid,
  metric_set,
  checkpoint_dir,
  model_type,
  chunk_size = 5,
  n_train_rows = NULL,
  model_hash = NULL
) {
  if (!is.null(checkpoint_dir) && !dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE)
  }

  checkpoint_path <- get_checkpoint_path(
    checkpoint_dir,
    model_type,
    "grid",
    model_hash
  )
  current_metadata <- create_checkpoint_metadata(
    cv_splits,
    grid,
    n_train_rows,
    model_hash
  )
  checkpoint <- load_grid_checkpoint(checkpoint_path, current_metadata)

  grid_chunks <- split_grid(grid, chunk_size)
  n_chunks <- length(grid_chunks)

  results_list <- checkpoint$results_list
  start_chunk <- checkpoint$start_chunk

  if (start_chunk > n_chunks) {
    message("All grid chunks already completed, skipping to merge...")
    return(merge_tune_results(results_list))
  }

  if (start_chunk > 1) {
    message(sprintf(
      "Resuming from checkpoint: starting at chunk %d/%d",
      start_chunk,
      n_chunks
    ))
  }

  gridcontrol <- tune::control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "resamples"
  )

  for (i in seq(start_chunk, n_chunks)) {
    message(sprintf(
      "--- Grid chunk %d/%d (%d points) ---",
      i,
      n_chunks,
      nrow(grid_chunks[[i]])
    ))

    chunk_result <- tune::tune_grid(
      workflow,
      resamples = cv_splits,
      grid = grid_chunks[[i]],
      metrics = metric_set,
      control = gridcontrol
    )

    results_list[[i]] <- chunk_result
    save_grid_checkpoint(
      checkpoint_path,
      results_list,
      i,
      n_chunks,
      current_metadata
    )
  }

  merge_tune_results(results_list)
}

tune_bayes_with_checkpoints <- function(
  workflow,
  cv_splits,
  metric_set,
  initial_results,
  params,
  bayes_iterations,
  bayescontrol,
  checkpoint_path,
  metadata
) {
  checkpoint_interval <- max(1, floor(bayes_iterations / 5))

  current_results <- initial_results
  completed_iter <- 0

  while (completed_iter < bayes_iterations) {
    remaining <- bayes_iterations - completed_iter
    iter_chunk <- min(checkpoint_interval, remaining)

    message(sprintf(
      "Running Bayes iterations %d-%d of %d",
      completed_iter + 1,
      completed_iter + iter_chunk,
      bayes_iterations
    ))

    current_results <- tune::tune_bayes(
      workflow,
      resamples = cv_splits,
      metrics = metric_set,
      initial = current_results,
      param_info = params,
      iter = iter_chunk,
      control = bayescontrol
    )

    completed_iter <- completed_iter + iter_chunk

    save_bayes_checkpoint(
      checkpoint_path,
      current_results,
      completed_iter,
      bayes_iterations,
      metadata
    )
  }

  current_results
}

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
#' @param model_type Character: "rf" for random forest or "glmnet"
#' @param seed Random seed
#' @param n_cores_tuning Number of parallel workers for tuning
#' @param initial_grid_size Number of points for initial space-filling grid
#' @param bayes_iterations Number of Bayesian optimization iterations
#' @param uncertain_jump Uncertainty parameter for tune_bayes
#' @param checkpoint_dir Directory for saving checkpoints (NULL to disable)
#' @param grid_chunk_size Number of grid points per chunk for checkpointing
#' @param settings Full settings list (for hash computation)
#' @return List with tuning_results, best_params
tune_bayes_workflow <- function(
  workflow,
  train_df,
  cv_splits,
  params,
  model_type = "rf",
  seed = 42,
  n_cores_tuning = 4,
  initial_grid_size = 10,
  bayes_iterations = 15,
  uncertain_jump = 5,
  checkpoint_dir = "models/classifier/checkpoints",
  grid_chunk_size = 5,
  settings = NULL
) {
  n_folds <- length(cv_splits$splits)
  print_tuning_core_guidance(n_folds, initial_grid_size)
  metric_set <- create_tuning_metrics()

  initial_grid <- dials::grid_space_filling(
    params,
    size = initial_grid_size,
    type = "latin_hypercube"
  )

  model_hash <- if (!is.null(settings)) {
    compute_model_hash(train_df, cv_splits, initial_grid, settings, model_type)
  } else {
    NULL
  }

  if (!is.null(model_hash)) {
    message(sprintf("Model hash: %s", substr(model_hash, 1, 8)))
    log_model_hash_info(
      model_hash,
      train_df,
      cv_splits,
      initial_grid,
      settings,
      model_type,
      checkpoint_dir
    )
  }

  bayescontrol <- tune::control_bayes(
    verbose = TRUE,
    verbose_iter = TRUE,
    no_improve = 10,
    seed = seed,
    uncertain = uncertain_jump,
    parallel_over = "resamples"
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
  print_phase_info("INITIAL GRID TUNING (chunked)", n_cores_tuning)

  initial_results <- tune_grid_chunked(
    workflow = workflow,
    cv_splits = cv_splits,
    grid = initial_grid,
    metric_set = metric_set,
    checkpoint_dir = checkpoint_dir,
    model_type = model_type,
    chunk_size = grid_chunk_size,
    n_train_rows = nrow(train_df),
    model_hash = model_hash
  )

  print_phase_info("BAYESIAN OPTIMIZATION", n_cores_tuning)

  bayes_checkpoint_path <- get_checkpoint_path(
    checkpoint_dir,
    model_type,
    "bayes",
    model_hash
  )
  bayes_metadata <- create_checkpoint_metadata(
    cv_splits,
    initial_grid,
    nrow(train_df),
    model_hash
  )
  bayes_checkpoint <- load_bayes_checkpoint(
    bayes_checkpoint_path,
    bayes_metadata
  )

  if (!is.null(bayes_checkpoint)) {
    current_results <- bayes_checkpoint$tuning_results
    start_iter <- bayes_checkpoint$completed_iter + 1
    remaining_iter <- bayes_iterations - bayes_checkpoint$completed_iter

    if (remaining_iter <= 0) {
      message("Bayesian optimization already complete from checkpoint")
      tuning_results <- current_results
    } else {
      message(sprintf(
        "Continuing Bayesian optimization: %d more iterations",
        remaining_iter
      ))
      tuning_results <- tune::tune_bayes(
        workflow,
        resamples = cv_splits,
        metrics = metric_set,
        initial = current_results,
        param_info = params,
        iter = remaining_iter,
        control = bayescontrol
      )

      save_bayes_checkpoint(
        bayes_checkpoint_path,
        tuning_results,
        bayes_iterations,
        bayes_iterations,
        bayes_metadata
      )
    }
  } else {
    tuning_results <- tune_bayes_with_checkpoints(
      workflow,
      cv_splits,
      metric_set,
      initial_results,
      params,
      bayes_iterations,
      bayescontrol,
      bayes_checkpoint_path,
      bayes_metadata
    )
  }

  best_params <- select_best_by_complexity(tuning_results, model_type)
  message(
    "Best hyperparameters: ",
    paste(names(best_params), "=", unlist(best_params), collapse = ", ")
  )

  list(
    tuning_results = tuning_results,
    best_params = best_params,
    model_hash = model_hash
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

select_best_by_complexity <- function(tuning_results, model_type) {
  tuned_params <- get_tuned_param_names(tuning_results)
  complexity_order <- build_complexity_order(model_type, tuned_params)

  if (length(complexity_order) == 0) {
    return(tune::select_best(tuning_results, metric = "macro_f1_with_zeros"))
  }

  select_call <- rlang::call2(
    tune::select_by_one_std_err,
    tuning_results,
    metric = "macro_f1_with_zeros",
    !!!complexity_order
  )
  rlang::eval_tidy(select_call)
}

get_tuned_param_names <- function(tuning_results) {
  all_cols <- colnames(tune::collect_metrics(tuning_results))
  setdiff(
    all_cols,
    c(
      ".metric",
      ".estimator",
      "mean",
      "n",
      "std_err",
      ".config",
      ".iter"
    )
  )
}

build_complexity_order <- function(model_type, tuned_params) {
  rf_priority <- c("max.depth", "min_n", "mtry")
  rf_desc <- c(FALSE, TRUE, FALSE)

  glmnet_priority <- c("penalty", "mixture")
  glmnet_desc <- c(TRUE, FALSE)

  shared_priority <- c("over_ratio", "under_ratio")
  shared_desc <- c(FALSE, TRUE)

  if (model_type == "rf") {
    priority <- c(rf_priority, shared_priority)
    use_desc <- c(rf_desc, shared_desc)
  } else {
    priority <- c(glmnet_priority, shared_priority)
    use_desc <- c(glmnet_desc, shared_desc)
  }

  available <- priority %in% tuned_params
  priority <- priority[available]
  use_desc <- use_desc[available]

  purrr::map2(priority, use_desc, function(param, desc) {
    sym <- rlang::sym(param)
    if (desc) rlang::call2(dplyr::desc, sym) else sym
  }) |>
    stats::setNames(priority)
}
