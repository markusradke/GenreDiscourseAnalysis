#' Clean factor levels across CV folds and test set
#'
#' Identifies rare factor levels across all data partitions and remaps them to "other".
#' Removes factors that only contain "other" level after cleaning.
#'
#' @param cv_splits CV fold splits
#' @param train_data Training data
#' @param test_data Test data
#' @param min_n Minimum observations per level threshold
#' @return List with cleaned splits and datasets
clean_factor_levels_in_folds <- function(
  cv_splits,
  train_data,
  test_data,
  min_n = 100
) {
  message(sprintf(
    "Cleaning factor levels (threshold: %d)...",
    min_n
  ))

  factor_cols <- get_factor_columns(train_data)
  if (length(factor_cols) == 0) {
    return(list(
      cv_splits = cv_splits,
      train_data = train_data,
      test_data = test_data
    ))
  }

  rare_levels <- find_rare_levels_across_partitions(
    cv_splits,
    train_data,
    test_data,
    factor_cols,
    min_n
  )

  if (length(rare_levels) == 0) {
    return(list(
      cv_splits = cv_splits,
      train_data = train_data,
      test_data = test_data
    ))
  }

  log_rare_levels(rare_levels, min_n)

  train_data_remaped <- remap_rare_levels_in_data(train_data, rare_levels)
  test_data_remaped <- remap_rare_levels_in_data(test_data, rare_levels)
  cv_splits_remaped <- remap_rare_levels_in_splits(cv_splits, rare_levels)

  merge_result <- merge_small_other_with_smallest_level(
    cv_splits_remaped,
    train_data_remaped,
    test_data_remaped,
    factor_cols,
    min_n
  )

  train_data_merged <- merge_result$train_data
  test_data_merged <- merge_result$test_data
  cv_splits_merged <- merge_result$cv_splits

  single_level_factors <- find_single_level_factors(
    train_data_merged,
    factor_cols
  )
  if (length(single_level_factors) > 0) {
    log_single_level_removals(single_level_factors)
    train_data_final <- remove_columns(train_data_merged, single_level_factors)
    test_data_final <- remove_columns(test_data_merged, single_level_factors)
    cv_splits_final <- remove_columns_from_splits(
      cv_splits_merged,
      single_level_factors
    )
  } else {
    train_data_final <- train_data_merged
    test_data_final <- test_data_merged
    cv_splits_final <- cv_splits_merged
  }

  list(
    cv_splits = cv_splits_final,
    train_data = train_data_final,
    test_data = test_data_final
  )
}

merge_small_other_with_smallest_level <- function(
  cv_splits,
  train_data,
  test_data,
  factor_cols,
  min_n
) {
  levels_to_merge <- list()

  for (col in factor_cols) {
    if (!col %in% names(train_data)) {
      next
    }

    partitions <- collect_all_partitions(cv_splits, train_data, test_data, col)

    other_below_threshold <- FALSE
    if ("other" %in% levels(partitions[[1]])) {
      for (partition in partitions) {
        count <- sum(partition == "other", na.rm = TRUE)
        if (count < min_n) {
          other_below_threshold <- TRUE
          break
        }
      }
    }

    if (other_below_threshold) {
      all_levels <- levels(partitions[[1]])
      non_other_levels <- setdiff(all_levels, "other")

      if (length(non_other_levels) > 0) {
        level_min_counts <- setNames(
          rep(Inf, length(non_other_levels)),
          non_other_levels
        )

        for (level in non_other_levels) {
          for (partition in partitions) {
            count <- sum(partition == level, na.rm = TRUE)
            level_min_counts[level] <- min(level_min_counts[level], count)
          }
        }

        smallest_level <- names(level_min_counts)[which.min(level_min_counts)]
        message(sprintf(
          "  '%s': 'other' below threshold -> merging with '%s'",
          col,
          smallest_level
        ))

        levels_to_merge[[col]] <- smallest_level
      }
    }
  }

  if (length(levels_to_merge) > 0) {
    train_data <- remap_rare_levels_in_data(train_data, levels_to_merge)
    test_data <- remap_rare_levels_in_data(test_data, levels_to_merge)
    cv_splits <- remap_rare_levels_in_splits(cv_splits, levels_to_merge)
  }

  list(
    cv_splits = cv_splits,
    train_data = train_data,
    test_data = test_data
  )
}

get_factor_columns <- function(data) {
  factor_cols <- names(data)[vapply(data, is.factor, logical(1))]
  setdiff(factor_cols, "metagenre")
}

find_rare_levels_across_partitions <- function(
  cv_splits,
  train_data,
  test_data,
  factor_cols,
  min_n
) {
  rare_levels <- list()

  for (col in factor_cols) {
    partitions <- collect_all_partitions(cv_splits, train_data, test_data, col)
    rare <- identify_rare_levels_in_partitions(partitions, col, min_n)

    if (length(rare) > 0) {
      rare_levels[[col]] <- rare
    }
  }

  rare_levels
}

collect_all_partitions <- function(cv_splits, train_data, test_data, col) {
  partitions <- list()

  for (fold_idx in seq_len(nrow(cv_splits))) {
    analysis_data <- rsample::analysis(cv_splits$splits[[fold_idx]])
    assessment_data <- rsample::assessment(cv_splits$splits[[fold_idx]])
    partitions[[length(partitions) + 1]] <- analysis_data[[col]]
    partitions[[length(partitions) + 1]] <- assessment_data[[col]]
  }

  partitions[[length(partitions) + 1]] <- train_data[[col]]
  partitions[[length(partitions) + 1]] <- test_data[[col]]

  partitions
}

identify_rare_levels_in_partitions <- function(partitions, col, min_n) {
  all_levels <- levels(partitions[[1]])
  rare_levels <- character()

  for (level in all_levels) {
    for (partition in partitions) {
      count <- sum(partition == level, na.rm = TRUE)
      if (count < min_n) {
        rare_levels <- unique(c(rare_levels, level))
        break
      }
    }
  }

  rare_levels
}

log_rare_levels <- function(rare_levels, min_n) {
  for (col in names(rare_levels)) {
    for (level in rare_levels[[col]]) {
      message(sprintf(
        "  '%s' level '%s': < %d in at least one partition -> 'other'",
        col,
        level,
        min_n
      ))
    }
  }
}

remap_rare_levels_in_data <- function(data, rare_levels) {
  for (col in names(rare_levels)) {
    data <- remap_factor_column(data, col, rare_levels[[col]])
  }
  data
}

remap_factor_column <- function(data, col, rare_levels) {
  current_levels <- levels(data[[col]])
  if (!"other" %in% current_levels) {
    levels(data[[col]]) <- c(current_levels, "other")
  }

  data[[col]][data[[col]] %in% rare_levels] <- "other"
  data[[col]] <- droplevels(data[[col]])

  data
}

remap_rare_levels_in_splits <- function(cv_splits, rare_levels) {
  splits_list <- lapply(seq_len(nrow(cv_splits)), function(fold_idx) {
    split_obj <- cv_splits$splits[[fold_idx]]
    data_full <- split_obj$data

    # Remap each column with rare levels on the FULL data, not only analysis
    for (col in names(rare_levels)) {
      if (col %in% names(data_full)) {
        data_full <- remap_factor_column(data_full, col, rare_levels[[col]])
      }
    }

    analysis_idx <- split_obj$in_id
    assessment_idx <- split_obj$out_id

    rsample::make_splits(
      x = list(analysis = analysis_idx, assessment = assessment_idx),
      data = data_full
    )
  })

  rebuild_cv_splits(cv_splits, splits_list)
}

rebuild_cv_splits <- function(original_splits, new_splits_list) {
  cv_rset <- tibble::tibble(
    splits = new_splits_list,
    id = original_splits$id
  )

  if ("id2" %in% names(original_splits)) {
    cv_rset$id2 <- original_splits$id2
  }

  class(cv_rset) <- class(original_splits)
  attrs_to_copy <- attributes(original_splits)
  attrs_to_copy <- attrs_to_copy[
    !names(attrs_to_copy) %in% c("names", "row.names", "class")
  ]
  attributes(cv_rset) <- c(attributes(cv_rset), attrs_to_copy)

  cv_rset
}

find_single_level_factors <- function(data, factor_cols) {
  single_level <- character()

  for (col in factor_cols) {
    if (!col %in% names(data)) {
      next
    }

    remaining <- levels(data[[col]])
    if (length(remaining) == 1 && remaining[1] == "other") {
      single_level <- c(single_level, col)
    }
  }

  single_level
}

log_single_level_removals <- function(factor_names) {
  for (col in factor_names) {
    warning(
      sprintf("'%s' has only 'other' level - removing", col),
      call. = FALSE
    )
  }
}

remove_columns <- function(data, cols) {
  data |> dplyr::select(-dplyr::all_of(cols))
}

remove_columns_from_splits <- function(cv_splits, cols) {
  splits_list <- lapply(seq_len(nrow(cv_splits)), function(fold_idx) {
    split_obj <- cv_splits$splits[[fold_idx]]
    data_full <- split_obj$data

    # Remove columns from FULL data
    keep_cols <- setdiff(names(data_full), cols)
    data_full <- data_full[, keep_cols, drop = FALSE]

    analysis_idx <- split_obj$in_id
    assessment_idx <- split_obj$out_id

    rsample::make_splits(
      x = list(analysis = analysis_idx, assessment = assessment_idx),
      data = data_full
    )
  })

  rebuild_cv_splits(cv_splits, splits_list)
}
