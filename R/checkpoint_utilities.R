# Checkpoint management utilities

#' List all checkpoints in the checkpoint directory
#' @param checkpoint_dir Path to checkpoint directory
#' @return Data frame with checkpoint information
list_checkpoints <- function(checkpoint_dir = "models/classifier/checkpoints") {
  if (!dir.exists(checkpoint_dir)) {
    message("Checkpoint directory does not exist: ", checkpoint_dir)
    return(data.frame())
  }

  files <- list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    message("No checkpoint files found in: ", checkpoint_dir)
    return(data.frame())
  }

  checkpoint_info <- purrr::map_dfr(files, function(file) {
    tryCatch(
      {
        checkpoint <- readRDS(file)
        data.frame(
          file = basename(file),
          phase = checkpoint$phase %||% "unknown",
          timestamp = as.character(checkpoint$timestamp %||% NA),
          model_hash = substr(checkpoint$metadata$model_hash %||% "none", 1, 8),
          n_folds = checkpoint$metadata$n_folds %||% NA,
          n_train_rows = checkpoint$metadata$n_train_rows %||% NA,
          size_mb = round(file.size(file) / 1024^2, 2),
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        data.frame(
          file = basename(file),
          phase = "error",
          timestamp = NA,
          model_hash = "corrupt",
          n_folds = NA,
          n_train_rows = NA,
          size_mb = round(file.size(file) / 1024^2, 2),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  checkpoint_info <- checkpoint_info |>
    dplyr::arrange(dplyr::desc(timestamp))

  checkpoint_info
}

#' Remove checkpoints for a specific model hash
#' @param checkpoint_dir Path to checkpoint directory
#' @param model_hash Hash of the model (8 characters or full)
#' @param dry_run If TRUE, only show what would be deleted
#' @return Number of files removed
remove_checkpoints_by_hash <- function(
  checkpoint_dir = "models/classifier/checkpoints",
  model_hash,
  dry_run = TRUE
) {
  if (!dir.exists(checkpoint_dir)) {
    stop("Checkpoint directory does not exist: ", checkpoint_dir)
  }

  hash_short <- substr(model_hash, 1, 8)
  pattern <- sprintf("_%s_", hash_short)
  files <- list.files(
    checkpoint_dir,
    pattern = pattern,
    full.names = TRUE
  )

  if (length(files) == 0) {
    message(sprintf("No checkpoint files found for hash: %s", hash_short))
    return(0)
  }

  if (dry_run) {
    message(sprintf(
      "Would delete %d files for hash %s:",
      length(files),
      hash_short
    ))
    message(paste("  -", basename(files), collapse = "\n"))
    message("\nSet dry_run = FALSE to actually delete these files.")
    return(0)
  } else {
    n_removed <- sum(file.remove(files))
    message(sprintf(
      "Removed %d checkpoint files for hash: %s",
      n_removed,
      hash_short
    ))
    return(n_removed)
  }
}

#' Remove old checkpoints (keep only most recent N per model type)
#' @param checkpoint_dir Path to checkpoint directory
#' @param keep_n Number of most recent checkpoints to keep per model type
#' @param dry_run If TRUE, only show what would be deleted
#' @return Number of files removed
cleanup_old_checkpoints <- function(
  checkpoint_dir = "models/classifier/checkpoints",
  keep_n = 2,
  dry_run = TRUE
) {
  checkpoint_info <- list_checkpoints(checkpoint_dir)

  if (nrow(checkpoint_info) == 0) {
    return(0)
  }

  checkpoint_info <- checkpoint_info |>
    dplyr::filter(!is.na(timestamp)) |>
    dplyr::arrange(dplyr::desc(timestamp))

  model_types <- unique(
    stringr::str_extract(checkpoint_info$file, "^[a-z]+")
  )

  files_to_remove <- character(0)

  for (model_type in model_types) {
    type_files <- checkpoint_info |>
      dplyr::filter(stringr::str_starts(file, model_type)) |>
      dplyr::pull(file)

    if (length(type_files) > keep_n) {
      files_to_remove <- c(
        files_to_remove,
        type_files[(keep_n + 1):length(type_files)]
      )
    }
  }

  if (length(files_to_remove) == 0) {
    message(sprintf(
      "No old checkpoints to clean up (keeping %d most recent per model type)",
      keep_n
    ))
    return(0)
  }

  full_paths <- file.path(checkpoint_dir, files_to_remove)

  if (dry_run) {
    message(sprintf(
      "Would delete %d old checkpoint files (keeping %d most recent per type):",
      length(files_to_remove),
      keep_n
    ))
    message(paste("  -", files_to_remove, collapse = "\n"))
    message("\nSet dry_run = FALSE to actually delete these files.")
    return(0)
  } else {
    n_removed <- sum(file.remove(full_paths))
    message(sprintf(
      "Removed %d old checkpoint files (kept %d most recent per type)",
      n_removed,
      keep_n
    ))
    return(n_removed)
  }
}

#' Get checkpoint statistics
#' @param checkpoint_dir Path to checkpoint directory
#' @return List with summary statistics
checkpoint_summary <- function(
  checkpoint_dir = "models/classifier/checkpoints"
) {
  checkpoint_info <- list_checkpoints(checkpoint_dir)

  if (nrow(checkpoint_info) == 0) {
    return(list(
      n_files = 0,
      total_size_mb = 0,
      model_types = character(0),
      unique_hashes = character(0)
    ))
  }

  list(
    n_files = nrow(checkpoint_info),
    total_size_mb = sum(checkpoint_info$size_mb, na.rm = TRUE),
    model_types = unique(stringr::str_extract(checkpoint_info$file, "^[a-z]+")),
    unique_hashes = unique(checkpoint_info$model_hash[
      checkpoint_info$model_hash != "none"
    ]),
    by_phase = table(checkpoint_info$phase),
    oldest_checkpoint = min(checkpoint_info$timestamp, na.rm = TRUE),
    newest_checkpoint = max(checkpoint_info$timestamp, na.rm = TRUE)
  )
}

#' Read and display model hash information log
#' @param model_hash Hash of the model (8 characters or full)
#' @param model_type Model type (rf, glmnet, mars, rda)
#' @param checkpoint_dir Path to checkpoint directory
#' @return Character vector with log content (invisibly)
read_hash_info <- function(
  model_hash,
  model_type,
  checkpoint_dir = "models/classifier/checkpoints"
) {
  hash_short <- substr(model_hash, 1, 8)
  log_file <- file.path(
    checkpoint_dir,
    sprintf("%s_%s_info.txt", model_type, hash_short)
  )

  if (!file.exists(log_file)) {
    message(sprintf(
      "No info log found for %s hash %s",
      model_type,
      hash_short
    ))
    message(sprintf("Expected file: %s", log_file))
    return(invisible(NULL))
  }

  log_content <- readLines(log_file)
  cat(log_content, sep = "\n")
  invisible(log_content)
}

#' List all available hash info logs
#' @param checkpoint_dir Path to checkpoint directory
#' @return Data frame with hash info file details
list_hash_info_logs <- function(
  checkpoint_dir = "models/classifier/checkpoints"
) {
  if (!dir.exists(checkpoint_dir)) {
    message("Checkpoint directory does not exist: ", checkpoint_dir)
    return(data.frame())
  }

  files <- list.files(
    checkpoint_dir,
    pattern = "_info\\.txt$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    message("No hash info logs found in: ", checkpoint_dir)
    return(data.frame())
  }

  info <- purrr::map_dfr(files, function(file) {
    parts <- strsplit(basename(file), "_")[[1]]
    model_type <- parts[1]
    hash <- parts[2]

    data.frame(
      file = basename(file),
      model_type = model_type,
      hash = hash,
      created = file.info(file)$mtime,
      size_kb = round(file.size(file) / 1024, 2),
      stringsAsFactors = FALSE
    )
  })

  info |> dplyr::arrange(dplyr::desc(created))
}
