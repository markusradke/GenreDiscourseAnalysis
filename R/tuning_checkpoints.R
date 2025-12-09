# Checkpoint management for hyperparameter tuning

compute_model_hash <- function(
  train_df,
  cv_splits,
  grid,
  settings,
  model_type
) {
  train_structure <- list(
    nrow = nrow(train_df),
    ncol = ncol(train_df),
    colnames = colnames(train_df),
    class_dist = as.list(table(train_df$metagenre))
  )

  cv_structure <- list(
    n_folds = length(cv_splits$splits),
    fold_sizes = purrr::map_int(cv_splits$splits, ~ nrow(rsample::analysis(.x)))
  )

  tuning_settings <- settings[grep(
    "^(tune_|.*_fix$|seed|model_features)",
    names(settings)
  )]

  hash_input <- list(
    train = train_structure,
    cv = cv_structure,
    grid = grid,
    settings = tuning_settings,
    model_type = model_type
  )

  digest::digest(hash_input, algo = "md5")
}

log_model_hash_info <- function(
  model_hash,
  train_df,
  cv_splits,
  grid,
  settings,
  model_type,
  checkpoint_dir = "models/classifier/checkpoints"
) {
  if (is.null(model_hash) || is.null(checkpoint_dir)) {
    return(invisible(NULL))
  }

  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE)
  }

  hash_short <- substr(model_hash, 1, 8)
  log_file <- file.path(
    checkpoint_dir,
    sprintf("%s_%s_info.txt", model_type, hash_short)
  )

  if (file.exists(log_file)) {
    return(invisible(NULL))
  }

  class_dist <- table(train_df$metagenre)
  n_levels <- length(class_dist)

  tuning_settings <- settings[grep(
    "^(tune_|.*_fix$|seed|model_features)",
    names(settings)
  )]

  log_content <- c(
    "=" |> rep(70) |> paste(collapse = ""),
    sprintf("MODEL HASH INFORMATION LOG"),
    "=" |> rep(70) |> paste(collapse = ""),
    "",
    sprintf("Generated: %s", Sys.time()),
    sprintf("Model Type: %s", model_type),
    sprintf("Hash (full): %s", model_hash),
    sprintf("Hash (short): %s", hash_short),
    "",
    "-" |> rep(70) |> paste(collapse = ""),
    "TRAINING DATA STRUCTURE",
    "-" |> rep(70) |> paste(collapse = ""),
    sprintf("Number of rows: %d", nrow(train_df)),
    sprintf("Number of columns: %d", ncol(train_df)),
    sprintf("Number of features: %d", length(settings$model_features)),
    "",
    "Columns:",
    paste("  ", colnames(train_df)),
    "",
    sprintf("Outcome variable: metagenre (%d levels)", n_levels),
    "Class distribution:",
    paste(sprintf(
      "  %-20s: %5d (%.1f%%)",
      names(class_dist),
      class_dist,
      100 * class_dist / sum(class_dist)
    )),
    "",
    "-" |> rep(70) |> paste(collapse = ""),
    "CROSS-VALIDATION STRUCTURE",
    "-" |> rep(70) |> paste(collapse = ""),
    sprintf("Number of folds: %d", length(cv_splits$splits)),
    "Fold sizes:",
    paste(sprintf(
      "  Fold %d: %d rows",
      seq_along(cv_splits$splits),
      purrr::map_int(cv_splits$splits, ~ nrow(rsample::analysis(.x)))
    )),
    "",
    "-" |> rep(70) |> paste(collapse = ""),
    "TUNING GRID",
    "-" |> rep(70) |> paste(collapse = ""),
    sprintf("Grid dimensions: %d x %d", nrow(grid), ncol(grid)),
    sprintf("Parameters: %s", paste(colnames(grid), collapse = ", ")),
    "",
    "Grid summary:",
    paste(utils::capture.output(print(summary(grid))), collapse = "\n"),
    "",
    "-" |> rep(70) |> paste(collapse = ""),
    "TUNING SETTINGS",
    "-" |> rep(70) |> paste(collapse = ""),
    ""
  )

  for (name in sort(names(tuning_settings))) {
    value <- tuning_settings[[name]]
    if (is.character(value) && length(value) > 5) {
      log_content <- c(
        log_content,
        sprintf("%s: [%d features]", name, length(value)),
        paste("  First 5:", paste(head(value, 5), collapse = ", ")),
        "  ..."
      )
    } else if (
      is.numeric(value) ||
        is.logical(value) ||
        (is.character(value) && length(value) <= 5)
    ) {
      log_content <- c(
        log_content,
        sprintf("%s: %s", name, paste(value, collapse = ", "))
      )
    } else {
      log_content <- c(
        log_content,
        sprintf("%s: %s", name, class(value)[1])
      )
    }
  }

  log_content <- c(
    log_content,
    "",
    "=" |> rep(70) |> paste(collapse = ""),
    "END OF LOG",
    "=" |> rep(70) |> paste(collapse = "")
  )

  writeLines(log_content, log_file)
  message(sprintf("Model hash info logged: %s", basename(log_file)))

  invisible(log_file)
}

get_checkpoint_path <- function(
  checkpoint_dir,
  model_type,
  phase = "grid",
  model_hash = NULL,
  checkpoint_version = NULL
) {
  if (is.null(checkpoint_dir)) {
    return(NULL)
  }

  if (is.null(model_hash)) {
    filename <- sprintf("%s_%s_checkpoint.rds", model_type, phase)
  } else if (is.null(checkpoint_version)) {
    filename <- sprintf(
      "%s_%s_%s_checkpoint.rds",
      model_type,
      substr(model_hash, 1, 8),
      phase
    )
  } else {
    filename <- sprintf(
      "%s_%s_%s_v%d.rds",
      model_type,
      substr(model_hash, 1, 8),
      phase,
      checkpoint_version
    )
  }

  file.path(checkpoint_dir, filename)
}

create_checkpoint_metadata <- function(
  cv_splits,
  grid,
  n_train_rows,
  model_hash = NULL
) {
  grid_hash <- digest::digest(grid, algo = "md5")
  list(
    n_folds = length(cv_splits$splits),
    n_train_rows = n_train_rows,
    grid_hash = grid_hash,
    grid_nrow = nrow(grid),
    grid_ncol = ncol(grid),
    model_hash = model_hash
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

  rotate_checkpoint_files(checkpoint_path)

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
    basename(checkpoint_path)
  ))
}

rotate_checkpoint_files <- function(checkpoint_path) {
  base_path <- tools::file_path_sans_ext(checkpoint_path)
  ext <- tools::file_ext(checkpoint_path)

  v1_path <- paste0(base_path, "_v1.", ext)
  v2_path <- paste0(base_path, "_v2.", ext)

  if (file.exists(checkpoint_path)) {
    if (file.exists(v2_path)) {
      file.remove(v2_path)
    }
    if (file.exists(v1_path)) {
      file.rename(v1_path, v2_path)
    }
    file.rename(checkpoint_path, v1_path)
  }
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

  rotate_checkpoint_files(checkpoint_path)

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
    basename(checkpoint_path)
  ))
}

load_grid_checkpoint <- function(checkpoint_path, current_metadata) {
  checkpoint <- find_and_load_checkpoint(checkpoint_path, "grid")

  if (is.null(checkpoint)) {
    return(list(results_list = list(), start_chunk = 1))
  }

  validate_checkpoint_metadata(
    checkpoint$metadata,
    current_metadata,
    checkpoint$source_file
  )

  list(
    results_list = checkpoint$results_list,
    start_chunk = checkpoint$completed_chunk + 1
  )
}

find_and_load_checkpoint <- function(checkpoint_path, expected_phase) {
  if (is.null(checkpoint_path)) {
    return(NULL)
  }

  base_path <- tools::file_path_sans_ext(checkpoint_path)
  ext <- tools::file_ext(checkpoint_path)

  candidate_paths <- c(
    checkpoint_path,
    paste0(base_path, "_v1.", ext),
    paste0(base_path, "_v2.", ext)
  )

  for (path in candidate_paths) {
    if (!file.exists(path)) {
      next
    }

    tryCatch(
      {
        checkpoint <- readRDS(path)
        if (!is.null(checkpoint$phase) && checkpoint$phase == expected_phase) {
          message(sprintf(
            "Loaded checkpoint from: %s (timestamp: %s)",
            basename(path),
            checkpoint$timestamp
          ))
          checkpoint$source_file <- path
          return(checkpoint)
        }
      },
      error = function(e) {
        warning(sprintf(
          "Failed to load checkpoint %s: %s",
          basename(path),
          e$message
        ))
      }
    )
  }

  NULL
}

load_bayes_checkpoint <- function(checkpoint_path, current_metadata) {
  checkpoint <- find_and_load_checkpoint(checkpoint_path, "bayes")

  if (is.null(checkpoint)) {
    return(NULL)
  }

  validate_checkpoint_metadata(
    checkpoint$metadata,
    current_metadata,
    checkpoint$source_file
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

  if (
    !is.null(saved$model_hash) &&
      !is.null(current$model_hash) &&
      saved$model_hash != current$model_hash
  ) {
    mismatches <- c(
      mismatches,
      sprintf(
        "model_hash: saved=%s, current=%s",
        substr(saved$model_hash, 1, 8),
        substr(current$model_hash, 1, 8)
      )
    )
  }

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

cleanup_checkpoints <- function(checkpoint_dir, model_type, model_hash = NULL) {
  if (is.null(checkpoint_dir)) {
    return(invisible(NULL))
  }

  phases <- c("grid", "bayes")
  for (phase in phases) {
    checkpoint_path <- get_checkpoint_path(
      checkpoint_dir,
      model_type,
      phase,
      model_hash
    )

    if (is.null(checkpoint_path)) {
      next
    }

    base_path <- tools::file_path_sans_ext(checkpoint_path)
    ext <- tools::file_ext(checkpoint_path)

    all_versions <- c(
      checkpoint_path,
      paste0(base_path, "_v1.", ext),
      paste0(base_path, "_v2.", ext)
    )

    existing <- all_versions[file.exists(all_versions)]

    if (length(existing) > 0) {
      message(sprintf(
        "Checkpoints retained for %s_%s: %s",
        model_type,
        phase,
        paste(basename(existing), collapse = ", ")
      ))
    }
  }

  invisible(NULL)
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
