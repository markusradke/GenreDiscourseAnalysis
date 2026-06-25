plot_mean_prob_mb_for_cat <- function(
  mb,
  filtered_cat,
  P_mb,
  cat_states_mb,
  taxonomy_label,
  right_margin_pt = 0
) {
  chosen_k <- nrow(dplyr::distinct(dplyr::select(filtered_cat, tag_name)))
  mb_wide <- prepare_mb_data(mb, P_mb, cat_states_mb, chosen_k)
  cat_wide <- prepare_single_category_data(filtered_cat, taxonomy_label)
  mean_probabilities <- compute_mean_probabilities(
    cat_wide,
    mb_wide,
    taxonomy_label
  )
  gini_prevs <- get_gini_and_prevalences_cat(
    cat_wide,
    mb_wide,
    taxonomy_label,
    chosen_k
  )

  make_mean_prob_plot(
    mean_probabilities,
    gini_prevs,
    chosen_k,
    taxonomy_label,
    right_margin_pt
  )
}

prepare_mb_data <- function(mb, P_mb, cat_states_mb, chosen_k) {
  mb_track_map <- get_track_category_probabilities(
    P_mb,
    cat_states_mb,
    chosen_k
  )
  mb_cat <- add_track_map_to_long(mb, mb_track_map) |>
    dplyr::select(dplyr::all_of(c(colnames(mb_track_map), "track.s.id"))) |>
    as.data.frame()

  mb_cat <- mb_cat[!duplicated(mb_cat$track.s.id), ]
  mb_cat$cat <- NULL
  mb_cat$cat_prob <- NULL
  names(mb_cat) <- sub("^", "mb.", names(mb_cat))
  names(mb_cat)[names(mb_cat) == "mb.track.s.id"] <- "track.s.id"
  mb_cat
}

prepare_single_category_data <- function(filtered_cat, taxonomy_label) {
  cat_name <- paste0(tolower(taxonomy_label), "_cat")
  counts <- table(filtered_cat$tag_name)
  order <- data.frame(
    tag_name = names(counts),
    rank = rank(counts)
  )
  message("Preparing single category data for: ", taxonomy_label)
  cat_wide <- dplyr::left_join(
    as.data.frame(filtered_cat),
    order,
    by = "tag_name"
  ) |>
    dplyr::group_by(track.s.id) |>
    dplyr::slice_min(rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(track.s.id, tag_name) |>
    dplyr::rename(!!cat_name := tag_name)
  cat_wide
}

compute_mean_probabilities <- function(cat_wide, mb_wide, taxonomy_label) {
  cat_name <- paste0(tolower(taxonomy_label), "_cat")
  mb_genres <- setdiff(colnames(mb_wide), "track.s.id")

  dplyr::inner_join(cat_wide, mb_wide, by = "track.s.id") |>
    dplyr::group_by(.data[[cat_name]]) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(mb_genres),
        function(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(mb_genres),
      names_to = "mb_cat",
      values_to = "mean_prob"
    ) |>
    dplyr::mutate(mb_cat = stringr::str_remove(.data$mb_cat, "mb\\."))
}

get_gini_and_prevalences_cat <- function(
  cat_wide,
  mb_wide,
  taxonomy_label,
  chosen_k
) {
  merged <- dplyr::inner_join(cat_wide, mb_wide, by = "track.s.id")
  gini_prevs_mb <- compute_gini_and_prevs_from_wide(mb_wide, merged)
  prevs_mb <- gini_prevs_mb$prevs
  names(prevs_mb) <- stringr::str_remove(names(prevs_mb), "mb\\.")
  prevs_cat <- merged |>
    pull(paste0(tolower(taxonomy_label), "_cat")) |>
    table() |>
    prop.table()
  gini_cat <- DescTools::Gini(prevs_cat)

  gini_label <- sprintf(
    "N=%s<br>Gini<sub>G%s</sub> = %.3f<br>Gini<sub>%s</sub> = %.3f",
    format(nrow(merged), big.mark = ","),
    chosen_k,
    gini_prevs_mb$gini,
    taxonomy_label,
    gini_cat
  )
  prevs <- list(
    mb = prevs_mb,
    cat = prevs_cat
  )
  list(gini_label = gini_label, prevs = prevs)
}

make_mean_prob_plot <- function(
  mean_probabilities,
  gini_prevs,
  chosen_k,
  taxonomy_label,
  right_margin_pt = 0
) {
  gini_label <- gini_prevs$gini_label
  prevs_mb <- gini_prevs$prevs$mb
  prevs_cat <- gini_prevs$prevs$cat

  cat_name <- paste0(tolower(taxonomy_label), "_cat")
  y_col <- mean_probabilities[[cat_name]]
  mean_probabilities$y_cat <- y_col
  mean_probabilities$text_color <- ifelse(
    mean_probabilities$mean_prob > 0.5,
    "white",
    "black"
  )
  mean_probabilities$y_cat <- paste0(
    mean_probabilities$y_cat,
    " (",
    round(prevs_cat[mean_probabilities$y_cat] * 100),
    "%)"
  )
  mean_probabilities$mb_cat <- paste0(
    mean_probabilities$mb_cat,
    " (",
    round(prevs_mb[mean_probabilities$mb_cat] * 100),
    "%)"
  )

  ggplot2::ggplot(
    mean_probabilities,
    ggplot2::aes(
      x = .data$mb_cat,
      y = .data$y_cat,
      fill = .data$mean_prob
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = round(.data$mean_prob * 100),
        color = .data$text_color
      ),
      size = 5
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "#28720aff") +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title = gini_label,
      x = paste0("MUSICMAP-G", chosen_k),
      y = taxonomy_label
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 16,
        face = "bold",
        color = "black",
        hjust = 0,
        vjust = -0.5,
        lineheight = 1.5,
        margin = ggplot2::margin(b = -72)
      ),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(
        t = 0,
        r = right_margin_pt,
        b = 0,
        l = 0,
        unit = "pt"
      ),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        angle = 70,
        hjust = 0,
        vjust = 0,
        size = 14
      ),
      axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0, size = 14),
      axis.title.x = ggplot2::element_text(hjust = 0),
      axis.title.y = ggplot2::element_text(hjust = 1),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        color = "grey45"
      )
    )
}
