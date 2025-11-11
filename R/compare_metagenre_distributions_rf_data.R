compare_metagenre_distributions <- function(
  datasets_list,
  detail_level = "low"
) {
  if (!detail_level %in% names(datasets_list)) {
    stop(sprintf("Dataset '%s' not found in datasets_list", detail_level))
  }

  dataset <- datasets_list[[detail_level]]
  all_data <- build_all_data(dataset)
  summary_stats <- compute_summary_stats(all_data)
  p <- make_plot(all_data, detail_level)

  list(
    plot = p,
    proportions = all_data
  )
}

make_plot <- function(all_data, detail_level) {
  ggplot2::ggplot(
    all_data,
    ggplot2::aes(
      x = metagenre,
      y = proportion,
      color = split_type,
      group = split_id
    )
  ) +
    ggplot2::geom_point(
      data = all_data |>
        dplyr::filter(
          split_type %in%
            c("CV_Analysis", "CV_Assessment")
        ),
      alpha = 0.4,
      size = 3
    ) +
    ggplot2::geom_point(
      data = all_data |> dplyr::filter(split_type == "Train"),
      size = 5,
      shape = 16
    ) +
    ggplot2::geom_point(
      data = all_data |> dplyr::filter(split_type == "Test"),
      size = 5,
      shape = 17
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Train" = "#2E7D32",
        "Test" = "#C62828",
        "CV_Analysis" = "#00ffeaff",
        "CV_Assessment" = "#f700ffff"
      ),
      labels = c(
        "Train" = "Training Set",
        "Test" = "Test Set",
        "CV_Analysis" = "CV Analysis Folds",
        "CV_Assessment" = "CV Assessment Folds"
      ),
      name = "Split Type"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = sprintf(
        "Metagenre Distribution Comparison (%s resolution)",
        toupper(detail_level)
      ),
      subtitle = "Comparing proportions across train, test, and CV splits",
      x = "Metagenre",
      y = "Proportion"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 14
      ),
      axis.text.y = ggplot2::element_text(size = 12, color = "grey45"),
      axis.title = ggplot2::element_text(
        size = 14,
        color = "grey45"
      ),
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(size = 12, color = "grey45"),
      legend.title = ggplot2::element_text(
        face = "bold",
        size = 14,
        color = "grey45"
      ),
      legend.text = ggplot2::element_text(color = "grey45", size = 14),
      legend.position = "top",
      legend.justification = "left",
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_all_data <- function(dataset) {
  train_counts <- table(dataset$train$metagenre)
  test_counts <- table(dataset$test$metagenre)

  train_df <- build_split_df(train_counts, "Train", "Train")
  test_df <- build_split_df(test_counts, "Test", "Test")
  cv_dfs <- extract_cv_dfs(dataset$cv_splits)

  dplyr::bind_rows(train_df, test_df, cv_dfs)
}

extract_cv_dfs <- function(cv_splits) {
  if (is.null(cv_splits)) {
    return(list())
  }
  result <- list()
  for (i in seq_along(cv_splits)) {
    sp <- cv_splits[[i]]
    split_id <- paste0(sp$fold, "_", sp$id2)

    analysis_counts <- table(sp$analysis$metagenre)
    result[[length(result) + 1]] <-
      build_split_df(analysis_counts, "CV_Analysis", split_id)

    assessment_counts <- table(sp$assessment$metagenre)
    result[[length(result) + 1]] <-
      build_split_df(assessment_counts, "CV_Assessment", split_id)
  }
  result
}

build_split_df <- function(counts, split_type, split_id) {
  data.frame(
    metagenre = names(counts),
    count = as.numeric(counts),
    proportion = as.numeric(counts) / sum(counts),
    split_type = split_type,
    split_id = split_id,
    stringsAsFactors = FALSE
  )
}

compute_summary_stats <- function(all_data) {
  all_data |>
    dplyr::group_by(metagenre, split_type) |>
    dplyr::summarize(
      mean_prop = mean(proportion),
      sd_prop = sd(proportion),
      min_prop = min(proportion),
      max_prop = max(proportion),
      n_splits = dplyr::n(),
      .groups = "drop"
    )
}
