get_oob_error_curve <- function(
  fit,
  recipe = NULL,
  train_data = NULL,
  metric = NULL,
  step = 10L,
  mode = c("auto", "regression", "classification")
) {
  mode <- match.arg(mode)
  rw <- extract_ranger_and_workflow(fit)
  rgr <- rw$rgr
  wf <- rw$wf

  if (is.null(rgr$inbag.counts)) {
    stop(
      "Ranger fit lacks inbag.counts. ",
      "Please fit the model with set_engine('ranger', keep.inbag = TRUE)."
    )
  }

  train_baked <- prepare_training_data(fit, wf, recipe, train_data)
  y <- infer_outcome(rgr, train_baked)

  mode <- determine_problem_mode(mode, rgr)
  metric_info <- choose_metric(metric, mode)
  metric_fun <- metric_info$fun
  metric_name <- metric_info$name

  check_predictor_vars(rgr, train_baked)

  all_tree_preds <- compute_all_tree_predictions(rgr, train_baked, y, mode)

  inbag_mat <- do.call(cbind, rgr$inbag.counts)
  n_trees <- ncol(inbag_mat)
  tree_index <- seq(1L, n_trees, by = step)

  oob_vals <- compute_oob_values(
    tree_index,
    all_tree_preds,
    inbag_mat,
    y,
    mode,
    metric_fun
  )

  metric_plot_name <- metric_name
  if (mode == "classification" && identical(metric_name, "kap")) {
    oob_vals <- 1 - oob_vals
    metric_plot_name <- "1 - kap"
  }

  curve_tbl <- tibble::tibble(
    trees = tree_index,
    !!metric_plot_name := oob_vals
  )

  subtitle_txt <- format_model_params(rgr, wf, mode)

  p <- ggplot2::ggplot(
    curve_tbl,
    ggplot2::aes(x = trees, y = .data[[metric_plot_name]])
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "OOB Error Curve",
      subtitle = subtitle_txt,
      x = "Number of Trees",
      y = metric_plot_name
    ) +
    ggplot2::theme_minimal()

  invisible(list(curve = curve_tbl, plot = p))
}

#--------------------------------------------------------------
# Ranger-Objekt + Workflow aus einem Fit-Objekt ziehen
#--------------------------------------------------------------
extract_ranger_and_workflow <- function(x) {
  if (
    "last_fit" %in%
      class(x) ||
      "tune_results" %in% class(x) ||
      (inherits(x, "tbl_df") && ".workflow" %in% names(x))
  ) {
    wf <- x$.workflow[[1]]
    rgr <- workflows::extract_fit_engine(wf)
    return(list(rgr = rgr, wf = wf))
  }

  if (inherits(x, "workflow") || inherits(x, "workflow_fit")) {
    rgr <- workflows::extract_fit_engine(x)
    return(list(rgr = rgr, wf = x))
  }

  if (inherits(x, "model_fit") && inherits(x$fit, "ranger")) {
    return(list(rgr = x$fit, wf = NULL))
  }

  if (inherits(x, "ranger")) {
    return(list(rgr = x, wf = NULL))
  }

  stop("Unsupported fit object for oob_error_curve().")
}

prepare_training_data <- function(fit, wf, recipe = NULL, train_data = NULL) {
  if (!is.null(train_data)) {
    return(train_data)
  }

  if ("last_fit" %in% class(fit)) {
    split_obj <- fit$splits[[1]]
    train_orig <- rsample::training(split_obj)

    if (is.null(recipe)) {
      recipe <- workflows::extract_recipe(wf, estimated = FALSE)
    }

    rec_prep <- recipes::prep(recipe, training = train_orig, retain = TRUE)
    return(recipes::juice(rec_prep))
  }

  if (!is.null(recipe)) {
    td <- try(recipes::bake(recipe, new_data = NULL), silent = TRUE)
    if (!inherits(td, "try-error")) {
      return(td)
    }
    stop("Provided recipe does not appear to be prepped.")
  }

  if (!is.null(wf)) {
    rec <- try(workflows::extract_recipe(wf, estimated = TRUE), silent = TRUE)
    if (!inherits(rec, "try-error")) {
      td <- try(recipes::bake(rec, new_data = NULL), silent = TRUE)
      if (!inherits(td, "try-error")) return(td)
    }
  }

  stop(
    "Could not determine training data automatically. ",
    "Please supply train_data or a prepped recipe."
  )
}

infer_outcome <- function(rgr, train_baked) {
  if (!is.null(rgr$dependent.variable)) {
    return(rgr$dependent.variable)
  }
  if ("outcome" %in% names(train_baked)) {
    return(train_baked$outcome)
  }
  stop(
    "Could not find an outcome vector:\n",
    "- rgr$dependent.variable is NULL and\n",
    "- train_data has no 'outcome' column."
  )
}

determine_problem_mode <- function(mode_arg, rgr) {
  if (mode_arg != "auto") {
    return(mode_arg)
  }
  tt <- tolower(rgr$treetype)
  if (grepl("regress", tt)) {
    return("regression")
  }
  if (grepl("class", tt) || grepl("prob", tt)) {
    return("classification")
  }
  stop("Unknown ranger treetype: ", rgr$treetype)
}

choose_metric <- function(metric, mode) {
  if (is.null(metric)) {
    if (mode == "regression") {
      return(list(fun = yardstick::rmse_vec, name = "rmse"))
    }
    return(list(fun = yardstick::kap_vec, name = "kap"))
  }
  list(fun = metric, name = deparse(substitute(metric)))
}

check_predictor_vars <- function(rgr, train_baked) {
  x_vars <- rgr$forest$independent.variable.names
  missing_x <- setdiff(x_vars, names(train_baked))
  if (length(missing_x) > 0) {
    stop(
      "Missing predictor variables: ",
      paste(missing_x, collapse = ", "),
      ". Recipe and data must match the model."
    )
  }
}

compute_all_tree_predictions <- function(rgr, train_baked, y, mode) {
  x_vars <- rgr$forest$independent.variable.names
  x_data <- train_baked[, x_vars, drop = FALSE]

  pred_all <- predict(rgr, data = x_data, predict.all = TRUE, type = "response")
  all_tree_preds <- pred_all$predictions

  if (mode == "classification") {
    if (!is.factor(y)) {
      y <- factor(y)
    }

    if (length(dim(all_tree_preds)) == 2L) {
      return(as.matrix(all_tree_preds))
    }

    if (length(dim(all_tree_preds)) == 3L) {
      return(prob_preds_to_labels(all_tree_preds, y))
    }

    stop("Unknown format for pred_all$predictions (classification).")
  }

  all_tree_preds
}

prob_preds_to_labels <- function(all_preds, y) {
  dims <- dim(all_preds)
  n_obs <- dims[1]
  n_class <- dims[2]
  n_trees_pred <- dims[3]

  if (n_trees_pred == 0L) {
    return(matrix(NA_character_, n_obs, 0))
  }

  class_levels <- levels(factor(y))
  all_tree_labels <- matrix(NA_character_, nrow = n_obs, ncol = n_trees_pred)

  for (t in seq_len(n_trees_pred)) {
    probs_t <- all_preds[,, t, drop = FALSE]
    probs_t <- matrix(probs_t, nrow = n_obs, ncol = n_class)
    idx_max <- max.col(probs_t, ties.method = "first")

    if (length(class_levels) == n_class) {
      all_tree_labels[, t] <- class_levels[idx_max]
    } else {
      all_tree_labels[, t] <- colnames(probs_t)[idx_max]
    }
  }

  all_tree_labels
}

compute_oob_values <- function(
  tree_index,
  all_tree_preds,
  inbag_mat,
  y,
  mode,
  metric_fun
) {
  purrr::map_dbl(tree_index, function(k) {
    preds_k <- all_tree_preds[, 1:k, drop = FALSE]
    inbag_k <- inbag_mat[, 1:k, drop = FALSE]

    preds_k[inbag_k > 0] <- NA

    if (mode == "regression") {
      oob_pred <- rowMeans(preds_k, na.rm = TRUE)
      valid <- is.finite(oob_pred)
      if (!any(valid)) {
        return(NA_real_)
      }
      return(metric_fun(truth = y[valid], estimate = oob_pred[valid]))
    }

    oob_pred <- apply(preds_k, 1, function(z) {
      z <- z[!is.na(z)]
      if (length(z) == 0) {
        return(NA_character_)
      }
      names(sort(table(z), decreasing = TRUE))[1]
    })
    oob_pred <- factor(oob_pred, levels = levels(factor(y)))
    valid <- !is.na(oob_pred)
    if (!any(valid)) {
      return(NA_real_)
    }

    truth_valid <- y[valid]
    est_valid <- oob_pred[valid]

    if (
      length(unique(truth_valid)) < 2L ||
        length(unique(est_valid)) < 2L
    ) {
      return(NA_real_)
    }

    metric_fun(truth = truth_valid, estimate = est_valid)
  })
}

format_model_params <- function(rgr, wf, mode) {
  if (is.null(wf)) {
    trees <- rgr$num.trees
    mtry <- rgr$mtry
    min_n <- rgr$min.node.size
    return(sprintf(
      "[ranger, %s; mtry=%s, trees=%s, min_n=%s]",
      mode,
      mtry,
      trees,
      min_n
    ))
  }

  spec <- try(workflows::extract_spec_parsnip(wf), silent = TRUE)
  if (inherits(spec, "try-error")) {
    trees <- rgr$num.trees
    mtry <- rgr$mtry
    min_n <- rgr$min.node.size
    return(sprintf(
      "[ranger, %s; mtry=%s, trees=%s, min_n=%s]",
      mode,
      mtry,
      trees,
      min_n
    ))
  }

  # Extract common parsnip args defensively
  engine <- try(spec$engine, silent = TRUE)
  trees <- try(
    if (!is.null(spec$args$trees)) spec$args$trees else rgr$num.trees,
    silent = TRUE
  )
  mtry <- try(
    if (!is.null(spec$args$mtry)) spec$args$mtry else rgr$mtry,
    silent = TRUE
  )
  min_n <- try(
    if (!is.null(spec$args$min_n)) spec$args$min_n else rgr$min.node.size,
    silent = TRUE
  )

  sprintf(
    "[parsnip:%s, mtry=%s, trees=%s, min_n=%s]",
    ifelse(inherits(engine, "try-error"), "ranger", engine),
    as.character(mtry),
    as.character(trees),
    as.character(min_n)
  )
}
