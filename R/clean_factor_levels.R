#' Clean factor levels across CV folds and test set
#'
#' Identifies rare factor levels across all data partitions and remaps them to
#' "other". Removes factors that only contain "other" level after cleaning.
#'
#' @param cv_splits CV fold splits
#' @param train_data Training data
#' @param test_data Test data
#' @param min_n Minimum observations per level threshold
#' @return List with cleaned splits and datasets
clean_factor_levels <- function(
  cv_splits,
  train_data,
  test_data,
  min_n = 100
) {
  message(sprintf("Cleaning factor levels (threshold: %d)...", min_n))
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
  train_remapped <- remap_rare_levels_in_data(train_data, rare_levels)
  test_remapped <- remap_rare_levels_in_data(test_data, rare_levels)
  cv_remapped <- remap_rare_levels_in_splits(cv_splits, rare_levels)
  merged <- merge_small_other_with_smallest_level(
    cv_remapped,
    train_remapped,
    test_remapped,
    factor_cols,
    min_n
  )
  train_merged <- merged$train_data
  test_merged <- merged$test_data
  cv_merged <- merged$cv_splits
  single_level <- find_single_level_factors(train_merged, factor_cols)
  if (length(single_level) > 0) {
    log_single_level_removals(single_level)
    train_final <- remove_columns(train_merged, single_level)
    test_final <- remove_columns(test_merged, single_level)
  } else {
    train_final <- train_merged
    test_final <- test_merged
  }
  list(train_data = train_final, test_data = test_final)
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
    parts <- collect_partitions_for_column(
      cv_splits,
      train_data,
      test_data,
      col
    )
    if (!"other" %in% levels(parts[[1]])) {
      next
    }
    other_below <- FALSE
    for (p in parts) {
      if (sum(p == "other", na.rm = TRUE) < min_n) {
        other_below <- TRUE
        break
      }
    }
    if (!other_below) {
      next
    }
    all_levels <- levels(parts[[1]])
    non_other <- setdiff(all_levels, "other")
    if (length(non_other) == 0) {
      next
    }
    min_counts <- setNames(rep(Inf, length(non_other)), non_other)
    for (lv in non_other) {
      for (p in parts) {
        min_counts[lv] <- min(min_counts[lv], sum(p == lv, na.rm = TRUE))
      }
    }
    smallest <- names(min_counts)[which.min(min_counts)]
    message(sprintf(
      "  '%s': 'other' below threshold -> merging with '%s'",
      col,
      smallest
    ))
    levels_to_merge[[col]] <- smallest
  }
  if (length(levels_to_merge) > 0) {
    train_data <- remap_rare_levels_in_data(train_data, levels_to_merge)
    test_data <- remap_rare_levels_in_data(test_data, levels_to_merge)
    cv_splits <- remap_rare_levels_in_splits(cv_splits, levels_to_merge)
  }
  list(cv_splits = cv_splits, train_data = train_data, test_data = test_data)
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
    parts <- collect_partitions_for_column(
      cv_splits,
      train_data,
      test_data,
      col
    )
    rare <- identify_rare_levels_in_partitions(parts, min_n)
    if (length(rare) > 0) rare_levels[[col]] <- rare
  }
  rare_levels
}

collect_partitions_for_column <- function(
  cv_splits,
  train_data,
  test_data,
  col
) {
  parts <- list()
  for (i in seq_len(nrow(cv_splits))) {
    analysis_data <- rsample::analysis(cv_splits$splits[[i]])
    assessment_data <- rsample::assessment(cv_splits$splits[[i]])
    parts[[length(parts) + 1]] <- analysis_data[[col]]
    parts[[length(parts) + 1]] <- assessment_data[[col]]
  }
  parts[[length(parts) + 1]] <- train_data[[col]]
  parts[[length(parts) + 1]] <- test_data[[col]]
  parts
}

identify_rare_levels_in_partitions <- function(partitions, min_n) {
  all_levels <- levels(partitions[[1]])
  rare_levels <- character()
  for (lv in all_levels) {
    for (p in partitions) {
      if (sum(p == lv, na.rm = TRUE) < min_n) {
        rare_levels <- unique(c(rare_levels, lv))
        break
      }
    }
  }
  rare_levels
}

get_factor_columns <- function(data) {
  factor_cols <- names(data)[vapply(data, is.factor, logical(1))]
  setdiff(factor_cols, "metagenre")
}

remap_rare_levels_in_data <- function(data, rare_levels) {
  for (col in names(rare_levels)) {
    data <- remap_factor_column(data, col, rare_levels[[col]])
  }
  data
}

remap_factor_column <- function(data, col, rare_vals) {
  if (!col %in% names(data)) {
    return(data)
  }
  current_levels <- levels(data[[col]])
  if (!"other" %in% current_levels) {
    levels(data[[col]]) <- c(current_levels, "other")
  }
  data[[col]][data[[col]] %in% rare_vals] <- "other"
  data[[col]] <- droplevels(data[[col]])
  data
}

remap_rare_levels_in_splits <- function(cv_splits, rare_levels) {
  new_splits <- lapply(seq_len(nrow(cv_splits)), function(i) {
    split_obj <- cv_splits$splits[[i]]
    data_full <- split_obj$data
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
  rebuild_cv_splits(cv_splits, new_splits)
}

rebuild_cv_splits <- function(original_splits, new_splits_list) {
  cv_rset <- tibble::tibble(splits = new_splits_list, id = original_splits$id)
  if ("id2" %in% names(original_splits)) {
    cv_rset$id2 <- original_splits$id2
  }
  class(cv_rset) <- class(original_splits)
  attrs <- attributes(original_splits)
  attrs <- attrs[!names(attrs) %in% c("names", "row.names", "class")]
  attributes(cv_rset) <- c(attributes(cv_rset), attrs)
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

log_rare_levels <- function(rare_levels, min_n) {
  for (col in names(rare_levels)) {
    for (lv in rare_levels[[col]]) {
      message(sprintf(
        "  '%s' level '%s': < %d in at least one partition -> 'other'",
        col,
        lv,
        min_n
      ))
    }
  }
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


extract_viable_factor_structure <- function(df) {
  factor_cols <- names(df)[vapply(df, is.factor, logical(1))]
  factor_cols <- setdiff(factor_cols, "metagenre")

  structure <- list(
    keep_cols = names(df),
    factor_levels = lapply(
      stats::setNames(factor_cols, factor_cols),
      function(col) levels(df[[col]])
    )
  )

  structure
}

apply_factor_structure <- function(df, structure) {
  if (!"case_wts" %in% names(df)) {
    structure$keep_cols <- setdiff(structure$keep_cols, "case_wts")
  }
  df_filtered <- df |> dplyr::select(dplyr::all_of(structure$keep_cols))

  for (col in names(structure$factor_levels)) {
    if (!col %in% names(df_filtered)) {
      next
    }

    valid_levels <- structure$factor_levels[[col]]
    df_filtered[[col]] <- factor(
      df_filtered[[col]],
      levels = valid_levels
    )

    invalid_mask <- is.na(df_filtered[[col]])
    if (any(invalid_mask)) {
      if ("other" %in% valid_levels) {
        df_filtered[[col]][invalid_mask] <- "other"
      } else {
        warning(sprintf(
          "Column '%s': %d obs have invalid levels or are NA, setting to NA",
          col,
          sum(invalid_mask)
        ))
      }
    }
  }

  df_filtered
}
