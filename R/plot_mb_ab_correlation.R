plot_mb_ab_correlation <- function(mb, ab, P_mb, cat_states_mb) {
  chosen_k <- nrow(dplyr::distinct(dplyr::select(ab, tag_name)))
  mb_wide <- prepare_mb(mb, P_mb, cat_states_mb, chosen_k)
  ab_wide <- prepare_ab(ab)
  x <- merge_mb_ab_data(ab_wide, mb_wide)

  gini_prevs_mb <- compute_gini_and_prevs_from_wide(mb_wide, x)
  gini_prevs_ab <- compute_gini_and_prevs_from_wide(ab_wide, x)
  gini_label <- format_gini_label(
    nrow(x),
    gini_prevs_mb$gini,
    gini_prevs_ab$gini
  )

  spearman_mat <- compute_spearman_matrix(x, ab_wide, mb_wide)
  mat_ord <- order_correlation_matrix(spearman_mat)
  plot_df <- prepare_plot_data(
    mat_ord,
    gini_prevs_ab$prevs,
    gini_prevs_mb$prevs
  )

  make_plot(plot_df, gini_label, mat_ord)
}

prepare_mb <- function(mb, P_mb, cat_states_mb, chosen_k) {
  mb_track_map <- get_track_category_probabilities(
    P_mb,
    cat_states_mb,
    chosen_k
  )
  add_track_map_to_long(mb, mb_track_map) |>
    dplyr::select(colnames(mb_track_map), track.s.id) |>
    dplyr::distinct(track.s.id, .keep_all = TRUE) |>
    dplyr::select(-cat, -cat_prob) |>
    dplyr::rename_with(~ paste0("mb.", .), -track.s.id) |>
    as.data.frame()
}

prepare_ab <- function(ab) {
  data.table::dcast(
    ab,
    track.s.id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  ) |>
    dplyr::rename_with(~ paste0("ab.", .), -track.s.id) |>
    as.data.frame()
}

merge_mb_ab_data <- function(ab_wide, mb_wide) {
  dplyr::inner_join(ab_wide, mb_wide, by = "track.s.id")
}

compute_gini_and_prevs_from_wide <- function(wide_df, x) {
  wide_df_filtered <- dplyr::inner_join(
    wide_df,
    dplyr::select(x, track.s.id),
    by = "track.s.id"
  ) |>
    dplyr::select(-track.s.id)
  cat <- colnames(wide_df_filtered)[apply(
    wide_df_filtered,
    1,
    which.max
  )]
  # pad with zeros if categories not present in the data
  all_cats <- colnames(wide_df)[colnames(wide_df) != "track.s.id"]
  prevs <- table(cat) |> prop.table()
  zero_prev_cats <- setdiff(all_cats, names(prevs))
  if (length(zero_prev_cats) > 0) {
    prevs <- c(prevs, setNames(rep(0, length(zero_prev_cats)), zero_prev_cats))
  }
  gini <- DescTools::Gini(prevs)
  list(gini = gini, prevs = prevs)
}

format_gini_label <- function(n, gini_mb, gini_ab) {
  sprintf(
    "N=%s<br>Gini<sub>G7</sub> = %.3f<br>Gini<sub>ROSA</sub> = %.3f",
    format(n, big.mark = ","),
    gini_mb,
    gini_ab
  )
}

compute_spearman_matrix <- function(x, ab_wide, mb_wide) {
  c1 <- setdiff(names(ab_wide), "track.s.id")
  c2 <- setdiff(names(mb_wide), "track.s.id")
  cor(
    x[c1],
    x[c2],
    method = "spearman",
    use = "pairwise.complete.obs"
  )
}

order_correlation_matrix <- function(spearman_mat) {
  colnames(spearman_mat) <- stringr::str_remove(
    colnames(spearman_mat),
    "mb."
  )
  rownames(spearman_mat) <- stringr::str_remove(
    rownames(spearman_mat),
    "ab."
  )
  ord <- corrplot:::corrMatOrder(
    spearman_mat,
    order = "hclust",
    hclust.method = "ward.D2"
  )
  spearman_mat[ord, ord]
}

prepare_plot_data <- function(mat_ord, ab_prevs, mb_prevs) {
  plot_df <- reshape2::melt(
    mat_ord,
    varnames = c("ab_cat", "mb_cat"),
    value.name = "corr"
  )
  names(ab_prevs) <- stringr::str_remove(names(ab_prevs), "ab.")
  names(mb_prevs) <- stringr::str_remove(names(mb_prevs), "mb.")
  plot_df$ab_cat <- factor(
    plot_df$ab_cat,
    levels = rownames(mat_ord),
    labels = paste0(
      rownames(mat_ord),
      " (",
      round(ab_prevs[rownames(mat_ord)] * 100),
      "%)"
    )
  )
  plot_df$mb_cat <- factor(
    plot_df$mb_cat,
    levels = colnames(mat_ord),
    labels = paste0(
      colnames(mat_ord),
      " (",
      round(mb_prevs[colnames(mat_ord)] * 100),
      "%)"
    )
  )

  plot_df
}

make_plot <- function(plot_df, gini_label, mat_ord) {
  ggplot2::ggplot(plot_df, ggplot2::aes(mb_cat, ab_cat, fill = corr)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = round(corr, 2)),
      size = 5,
      color = "black"
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#c40d20",
      mid = "white",
      high = "#28720aff",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = gini_label,
      x = "MUSICMAP-G7",
      y = "Rosamerica (AcousticBrainz)"
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
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        angle = 70,
        hjust = 0,
        vjust = 0,
        size = 14
      ),
      axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0, size = 14),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        color = "grey45"
      ),
      axis.title.x = ggplot2::element_text(hjust = 0),
      axis.title.y = ggplot2::element_text(hjust = 1)
    )
}
