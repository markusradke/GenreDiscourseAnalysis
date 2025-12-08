# Checkpoint management for hyperparameter tuning

get_checkpoint_path <- function(checkpoint_dir, model_type, phase = "grid") {
  if (is.null(checkpoint_dir)) {
    return(NULL)
  }
  file.path(
    checkpoint_dir,
    sprintf("%s_%s_checkpoint.rds", model_type, phase)
  )
}

create_checkpoint_metadata <- function(cv_splits, grid, n_train_rows) {
  grid_hash <- digest::digest(grid, algo = "md5")
  list(
    n_folds = length(cv_splits$splits),
    n_train_rows = n_train_rows,
    grid_hash = grid_hash,
    grid_nrow = nrow(grid),
    grid_ncol = ncol(grid)
  )
}

save_grid_checkpoint <- function(
  checkpoint_path,
  results_list,
  completed_chunk,
  n_chunks,
  metadata
) {
  if (is.null(checkpoint_path)) {
    return(invisible(NULL))
  }

  saveRDS(
    list(
      phase = "grid",
      results_list = results_list,
      completed_chunk = completed_chunk,
      metadata = metadata,
      timestamp = Sys.time()
    ),
    checkpoint_path
  )
  message(sprintf(
    "Checkpoint saved: %d/%d chunks complete (%s)",
    completed_chunk,
    n_chunks,
    checkpoint_path
  ))
}

save_bayes_checkpoint <- function(
  checkpoint_path,
  tuning_results,
  completed_iter,
  total_iter,
  metadata
) {
  if (is.null(checkpoint_path)) {
    return(invisible(NULL))
  }

  saveRDS(
    list(
      phase = "bayes",
      tuning_results = tuning_results,
      completed_iter = completed_iter,
      metadata = metadata,
      timestamp = Sys.time()
    ),
    checkpoint_path
  )
  message(sprintf(
    "Checkpoint saved: %d/%d Bayes iterations complete (%s)",
    completed_iter,
    total_iter,
    checkpoint_path
  ))
}

load_grid_checkpoint <- function(checkpoint_path, current_metadata) {
  if (is.null(checkpoint_path) || !file.exists(checkpoint_path)) {
    return(list(results_list = list(), start_chunk = 1))
  }

  checkpoint <- readRDS(checkpoint_path)

  if (!is.null(checkpoint$phase) && checkpoint$phase != "grid") {
    return(list(results_list = list(), start_chunk = 1))
  }

  validate_checkpoint_metadata(
    checkpoint$metadata,
    current_metadata,
    checkpoint_path
  )

  list(
    results_list = checkpoint$results_list,
    start_chunk = checkpoint$completed_chunk + 1
  )
}

load_bayes_checkpoint <- function(checkpoint_path, current_metadata) {
  if (is.null(checkpoint_path) || !file.exists(checkpoint_path)) {
    return(NULL)
  }

  checkpoint <- readRDS(checkpoint_path)

  if (is.null(checkpoint$phase) || checkpoint$phase != "bayes") {
    return(NULL)
  }

  validate_checkpoint_metadata(
    checkpoint$metadata,
    current_metadata,
    checkpoint_path
  )

  message(sprintf(
    "Resuming Bayesian optimization from iteration %d",
    checkpoint$completed_iter
  ))

  list(
    tuning_results = checkpoint$tuning_results,
    completed_iter = checkpoint$completed_iter
  )
}

validate_checkpoint_metadata <- function(saved, current, checkpoint_path) {
  mismatches <- character(0)

  if (!is.null(saved$n_folds) && saved$n_folds != current$n_folds) {
    mismatches <- c(
      mismatches,
      sprintf("n_folds: saved=%d, current=%d", saved$n_folds, current$n_folds)
    )
  }

  if (
    !is.null(saved$n_train_rows) &&
      !is.null(current$n_train_rows) &&
      saved$n_train_rows != current$n_train_rows
  ) {
    mismatches <- c(
      mismatches,
      sprintf(
        "n_train_rows: saved=%d, current=%d",
        saved$n_train_rows,
        current$n_train_rows
      )
    )
  }

  if (!is.null(saved$grid_hash) && saved$grid_hash != current$grid_hash) {
    mismatches <- c(
      mismatches,
      sprintf(
        "grid_hash mismatch (grid: saved=%dx%d, current=%dx%d)",
        saved$grid_nrow,
        saved$grid_ncol,
        current$grid_nrow,
        current$grid_ncol
      )
    )
  }

  if (length(mismatches) > 0) {
    stop(sprintf(
      "Checkpoint mismatch (%s):\n  - %s\n%s",
      checkpoint_path,
      paste(mismatches, collapse = "\n  - "),
      "Delete checkpoint to start fresh or ensure settings match."
    ))
  }

  invisible(TRUE)
}

cleanup_checkpoints <- function(checkpoint_dir, model_type) {
  phases <- c("grid", "bayes")
  for (phase in phases) {
    checkpoint_path <- get_checkpoint_path(checkpoint_dir, model_type, phase)
    if (!is.null(checkpoint_path) && file.exists(checkpoint_path)) {
      file.remove(checkpoint_path)
      message(sprintf("Checkpoint cleaned up: %s", checkpoint_path))
    }
  }
}

split_grid <- function(grid, chunk_size) {
  n_rows <- nrow(grid)
  chunk_indices <- ceiling(seq_len(n_rows) / chunk_size)
  split(grid, chunk_indices)
}

merge_tune_results <- function(results_list) {
  if (length(results_list) == 1) {
    return(results_list[[1]])
  }

  base_result <- results_list[[1]]

  for (i in seq(2, length(results_list))) {
    additional <- results_list[[i]]
    base_result$.metrics <- purrr::map2(
      base_result$.metrics,
      additional$.metrics,
      dplyr::bind_rows
    )
    if (".predictions" %in% names(base_result)) {
      base_result$.predictions <- purrr::map2(
        base_result$.predictions,
        additional$.predictions,
        dplyr::bind_rows
      )
    }
  }

  base_result
}
