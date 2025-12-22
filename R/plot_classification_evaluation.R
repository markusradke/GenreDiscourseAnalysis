#' Plot confusion matrix from evaluation object
#' @param evaluation Evaluation object with confusion matrices
#' @param set Either "train" or "test"
#' @export
plot_cm <- function(evaluation, certainty, set = "test") {
  if (set == "train") {
    cm_df <- evaluation$confusion_train
    metrics <- evaluation$metrics_train
  } else if (set == "test") {
    cm_df <- evaluation$confusion_test
    metrics <- evaluation$metrics_test
  } else {
    stop("set must be either 'train' or 'test'")
  }
  certainty_vec <- certainty$certainty
  names(certainty_vec) <- certainty$metagenre
  f1_scores <- calculate_class_f1_scores_from_confusion_df(cm_df)
  cm_df <- cm_df |>
    dplyr::mutate(
      Actual_label = sprintf(
        "%s (F1: %.0f%%, Certain: %.0f%%)",
        Actual,
        f1_scores[Actual] * 100,
        certainty_vec[Actual] * 100
      )
    )

  ggplot2::ggplot(
    cm_df,
    ggplot2::aes(
      x = Predicted,
      y = forcats::fct_rev(Actual_label),
      fill = relfreq
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.0f", relfreq * 100),
        color = labelcolor
      ),
      size = 14 / ggplot2::.pt,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#3e578e",
      labels = scales::percent_format(accuracy = 1),
      name = ""
    ) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = "PREDICTED",
      y = "ACTUAL",
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(color = "grey45", size = 16),
      legend.title = ggplot2::element_text(color = "grey45", size = 16),
      axis.text.x = ggplot2::element_text(
        angle = 65,
        hjust = 0,
        vjust = 0,
        size = 16
      ),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(
        color = "grey45",
        size = 18,
        hjust = 0
      ),
      axis.title.y = ggplot2::element_text(
        color = "grey45",
        size = 18,
        hjust = 1
      )
    )
}


get_metrics_description <- function(metrics) {
  sprintf(
    "F1-macro: %.3f Kappa: %.3f  Acc (unbalanced): %.3f",
    metrics$f1macro,
    metrics$kappa,
    metrics$accuracy
  )
}

plot_varimp <- function(varimp_df, top_n = 40) {
  df <- varimp_df |>
    dplyr::arrange(dplyr::desc(Importance)) |>
    head(top_n)
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = reorder(Variable, Importance),
      y = Importance
    )
  ) +
    ggplot2::geom_col(fill = "grey50") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = "Variable",
      y = "Importance"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(color = "grey45"),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(color = "grey45"),
      axis.title.y = ggplot2::element_blank()
    )
  p
}


show_important_predictors_glmnet <- function(glmnet_model) {
  important_coefs <- broom::tidy(
    glmnet_model$fit$fit
  ) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::group_by(class) |>
    dplyr::arrange(dplyr::desc(abs(estimate))) |>
    dplyr::slice_head(n = 5) |>
    dplyr::ungroup() |>
    dplyr::mutate(estimate = round(estimate, 2)) |>
    dplyr::select(-penalty)
  flextable::flextable(important_coefs) |>
    flextable::autofit() |>
    flextable::merge_v(j = "class") |>
    # horizontal line every 5 rows
    flextable::hline(
      i = seq(5, nrow(important_coefs), by = 5),
      border = officer::fp_border(width = 1)
    )
}

make_axis_title <- function(x) {
  with_space <- gsub("[_\\.]", " ", x)
  split <- strsplit(with_space, " ")[[1]]
  paste(
    toupper(substring(split, 1, 1)),
    substring(split, 2),
    sep = "",
    collapse = " "
  )
}

#' Prepare tuning top models with ranking and model grouping
#' @param tuning_history Tuning history data frame
#' @param chosen_params Named vector of chosen parameter values
#' @param top_n Number of top models to return
prepare_tuning_topmodels <- function(tuning_history, chosen_params, top_n) {
  param_names <- names(chosen_params)

  topmodels <- tuning_history |>
    dplyr::mutate(
      is_chosen = check_params_match(.data, chosen_params, param_names)
    ) |>
    dplyr::arrange(dplyr::desc(macro_f1_with_zeros_mean)) |>
    head(top_n) |>
    dplyr::mutate(rank = dplyr::row_number())

  best_f1 <- topmodels$macro_f1_with_zeros_mean[1]
  best_f1_se <- topmodels$macro_f1_with_zeros_std_err[1]

  topmodels |>
    dplyr::mutate(
      within_se_rule = macro_f1_with_zeros_mean >= (best_f1 - best_f1_se),
      modelgroup = dplyr::case_when(
        is_chosen ~ 1,
        within_se_rule ~ 2,
        TRUE ~ 3
      ) |>
        as.factor()
    )
}

check_params_match <- function(data, chosen_params, param_names) {
  match_expr <- lapply(param_names, function(p) {
    data[[p]] == chosen_params[[p]]
  })
  Reduce(`&`, match_expr)
}

#' Plot tuning model selection graph
#' @param topmodels Prepared top models from prepare_tuning_topmodels
#' @param top_n Number of models shown
plot_tuning_model_selection <- function(topmodels, top_n) {
  ggplot2::ggplot(
    topmodels,
    ggplot2::aes(x = rank, y = macro_f1_with_zeros_mean, color = modelgroup)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = macro_f1_with_zeros_mean - macro_f1_with_zeros_std_err,
        ymax = macro_f1_with_zeros_mean + macro_f1_with_zeros_std_err
      ),
      width = 0.2
    ) +
    ggplot2::labs(x = "Rank", y = "F1 Macro (mean ± se)") +
    ggplot2::scale_color_manual(values = c("#23cf00ff", "grey25", "grey75")) +
    ggplot2::scale_x_discrete(
      position = "top",
      limits = factor(seq_len(top_n))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_text(
        color = "grey45",
        size = 14,
        hjust = 1
      ),
      axis.title.x = ggplot2::element_text(
        color = "grey45",
        size = 14,
        hjust = 0
      ),
      axis.text.x = ggplot2::element_text(color = "grey45", size = 12),
      axis.text.y = ggplot2::element_text(
        color = "black",
        size = 12,
        face = "bold"
      ),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

#' Plot parameter tuning exploration
#' @param tuning_history Tuning history data frame
#' @param parameter Parameter name to plot
#' @param show_legend Whether to show the legend
plot_parameter_tuning <- function(
  tuning_history,
  parameter,
  show_legend = FALSE
) {
  ggplot2::ggplot(
    tuning_history,
    ggplot2::aes(
      x = .data[[parameter]],
      y = macro_f1_with_zeros_mean,
      color = .iter
    )
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::labs(x = make_axis_title(parameter), y = "F1 Macro (mean)") +
    ggplot2::scale_color_continuous(type = "viridis", name = "Iteration") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = if (show_legend) "top" else "none",
      axis.title.y = ggplot2::element_text(
        color = "grey45",
        size = 14,
        hjust = 1
      ),
      axis.title.x = ggplot2::element_text(
        color = "grey45",
        size = 14,
        hjust = 0
      ),
      axis.text.x = ggplot2::element_text(color = "grey45", size = 12),
      axis.text.y = ggplot2::element_text(
        color = "black",
        size = 12,
        face = "bold"
      )
    )
}

#' Create formatted tuning top table for LightGBM
#' @param topmodels Prepared top models
#' @param param_names Names of parameters to include in the table
#' @return Formatted top table data frame
create_lgbm_top_table <- function(topmodels, param_names) {
  topmodels |>
    dplyr::mutate(
      `F1 Macro (mean ± se)` = sprintf(
        "%.3f ± %.3f",
        macro_f1_with_zeros_mean,
        macro_f1_with_zeros_std_err
      ),
      `Kappa (mean ± se)` = sprintf("%.3f ± %.3f", kap_mean, kap_std_err),
      `Accuracy (mean ± se)` = sprintf(
        "%.3f ± %.3f",
        accuracy_mean,
        accuracy_std_err
      )
    ) |>
    dplyr::select(
      Rank = rank,
      Iteration = .iter,
      dplyr::all_of(param_names),
      `F1 Macro (mean ± se)`,
      `Kappa (mean ± se)`,
      `Accuracy (mean ± se)`,
      modelgroup
    )
}

#' Render tuning top table with kable styling
#' @param top_table Formatted table from create_*_top_table
#' @param col_names Column names for the table
render_tuning_top_table <- function(top_table) {
  knitr::kable(top_table) |>
    kableExtra::kable_styling(full_width = FALSE, position = "left") |>
    kableExtra::column_spec(
      1:(ncol(top_table) - 1),
      border_right = FALSE,
      border_left = FALSE
    ) |>
    kableExtra::row_spec(
      which(top_table$modelgroup == 1),
      bold = TRUE,
      background = "#23cf00ff"
    ) |>
    kableExtra::row_spec(
      which(top_table$modelgroup == 2),
      background = "grey90"
    ) |>
    kableExtra::row_spec(which(top_table$modelgroup == 3), color = "grey35") |>
    kableExtra::remove_column(ncol(top_table))
}

#' Plot variable importance bar chart
#' @param varimp_df Variable importance data frame with Variable and Importance columns
#' @param n_top Number of top variables to show
plot_varimp_bar <- function(varimp_df, n_top = 20) {
  varimp_df |>
    head(n_top) |>
    dplyr::arrange(dplyr::desc(Importance)) |>
    dplyr::mutate(
      Variable = forcats::fct_inorder(Variable) |> forcats::fct_rev()
    ) |>
    ggplot2::ggplot(ggplot2::aes(y = Variable, x = Importance)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey65") +
    ggplot2::scale_x_continuous(
      position = "top",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        color = "grey45",
        size = 16,
        hjust = 0
      ),
      axis.text.x = ggplot2::element_text(color = "grey45", size = 12),
      axis.text.y = ggplot2::element_text(color = "black", size = 12),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

#' Render GLMNET variable importance table
#' @param model GLMNET model object
#' @param n_top Number of top features per class
render_glmnet_varimp_table <- function(model, n_top = 5, caption = NULL) {
  broom::tidy(model) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::group_by(class) |>
    dplyr::slice_max(order_by = abs(estimate), n = n_top) |>
    dplyr::ungroup() |>
    dplyr::arrange(class, dplyr::desc(abs(estimate))) |>
    dplyr::mutate(estimate = round(estimate, 2)) |>
    dplyr::select(Metagenre = class, Feature = term, Coefficient = estimate) |>
    knitr::kable(
      col.names = c("Metagenre", "Feature", "Coefficient"),
      caption = caption
    ) |>
    kableExtra::kable_styling(full_width = FALSE, position = "left") |>
    kableExtra::column_spec(1:3, border_right = FALSE, border_left = FALSE) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kableExtra::collapse_rows(
      columns = 1,
      latex_hline = "major",
      valign = "top"
    )
}


plot_learner_comparison <- function(chosen_models) {
  label_df <- chosen_models |>
    dplyr::distinct(
      learner,
      detail,
      macro_f1_with_zeros_mean,
      macro_f1_with_zeros_std_err
    ) |>
    dplyr::mutate(
      color_group = ifelse(learner == "LightGBM", "LightGBM", "Other"),
      y_pos = dplyr::case_when(
        grepl("lowres", detail) ~ 0.65,
        grepl("mediumres", detail) ~ 0.52,
        TRUE ~ 0.45
      )
    )

  suppressWarnings(
    ggplot2::ggplot(
      chosen_models,
      ggplot2::aes(
        x = detail,
        y = macro_f1_with_zeros_mean,
        color = ifelse(learner == "LightGBM", "LightGBM", "Other"),
        group = learner
      )
    ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.5),
        ggplot2::aes(
          size = ifelse(learner == "LightGBM", "LightGBM", "Other")
        ),
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = macro_f1_with_zeros_mean - macro_f1_with_zeros_std_err,
          ymax = macro_f1_with_zeros_mean + macro_f1_with_zeros_std_err,
          color = ifelse(learner == "LightGBM", "LightGBM", "Other"),
          linewidth = ifelse(learner == "LightGBM", "LightGBM", "Other")
        ),
        width = 0.2,
        position = ggplot2::position_dodge(width = 0.5)
      ) +
      ggplot2::geom_text(
        data = label_df,
        ggplot2::aes(
          x = detail,
          y = y_pos,
          label = learner,
          color = color_group,
          fontface = ifelse(learner == "LightGBM", "bold", "plain"),
        ),
        vjust = 0.5,
        position = ggplot2::position_dodge(width = 0.5),
        size = 16 / ggplot2::.pt,
        show.legend = FALSE,
        angle = 45
      ) +
      ggplot2::scale_size_discrete(range = c(4, 3)) +
      ggplot2::scale_linewidth_discrete(range = c(1.5, 0.5)) +
      ggplot2::scale_color_manual(
        values = c("LightGBM" = "#3e578e", "Other" = "grey45")
      ) +
      ggplot2::scale_x_discrete(
        breaks = c("lowres", "mediumres"),
        labels = c("5 supergenres", "12 supergenres"),
        position = "bottom"
      ) +
      ggplot2::labs(x = "", y = "F1 macro") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = 16,
          color = "grey45",
          face = "bold"
        ),
        axis.title.y = ggplot2::element_text(
          size = 18,
          color = "grey45",
          hjust = 1
        ),
        axis.text.x = ggplot2::element_text(size = 18)
      )
  )
}

plot_gbm_varimp <- function(varimp, n_top) {
  prep <- varimp |>
    head(n_top) |>
    dplyr::mutate(
      Feature = forcats::fct_inorder(Feature) |> forcats::fct_rev(),
      level = dplyr::case_when(
        grepl("track", Feature) ~ "track",
        grepl("lyrics", Feature) ~ "track",
        grepl("artist", Feature) ~ "artist",
        grepl("dtb", Feature) ~ "artist",
        grepl("album", Feature) ~ "album",
        grepl("label", Feature) ~ "album"
      ),
      level = factor(level, levels = c("album", "artist", "track"))
    )
  ggplot2::ggplot(
    prep,
    ggplot2::aes(x = Gain, y = Feature, fill = level)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_continuous(
      position = "top",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "album" = "#c40d20",
        "artist" = "grey65",
        "track" = "#3e578eff"
      ),
      name = "Feature level"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # legend.position = c(0.8, 0.2),
      legend.position = "top",
      legend.text = ggplot2::element_text(
        size = 18,
        color = "grey45"
      ),
      legend.title = ggplot2::element_text(
        size = 18,
        color = "grey45"
      ),
      legend.justification = "left",
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        color = "grey45",
        size = 16,
        hjust = 0
      ),
      axis.text.x = ggplot2::element_text(color = "grey45", size = 16),
      axis.text.y = ggplot2::element_text(color = "black", size = 16),
      panel.grid.major.y = ggplot2::element_blank()
    )
}
