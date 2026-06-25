plot_gini_genre_categories <- function(ginis, xlim_max) {
  xlim_max <- min(length(ginis), xlim_max)
  no_genres <- length(ginis):1
  ggplot(
    data.frame(no_genres, gini = ginis),
    aes(x = no_genres, y = gini)
  ) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(
      title = "Gini Coefficient of Genre Size Distribution During Folding",
      x = "Step",
      y = "Gini Coefficient"
    ) +
    xlim(0, xlim_max)
}

inspect_k_categories_solution <- function(
  res_list,
  chosen_k,
  edgevis_thresh = 0.3
) {
  G <- nrow(res_list$mappings[[1]])
  message(sprintf(
    "Gini Latent Genre Categories:\n%.3f",
    res_list$ginis[[G - chosen_k]]
  ))
  final_mapping <- res_list$mappings[[G - chosen_k + 1]]
  final_sizes <- res_list$sizes[[G - chosen_k + 1]]
  final_sizes <- final_sizes[final_sizes > 0] |>
    sort(decreasing = TRUE)
  final_sizes_printout <- data.frame(
    Size = final_sizes
  )
  message("Final Latent Genre Sizes:")
  print(final_sizes_printout)

  final_genres <- names(final_sizes)
  final_weights <- res_list$weights[[G - chosen_k + 1]]
  final_weights <- final_weights[final_genres, final_genres]
  final_weights >= edgevis_thresh
  pruned_final_weights <- final_weights
  pruned_final_weights[is.na(pruned_final_weights)] <- 0
  pruned_final_weights[pruned_final_weights < edgevis_thresh] <- 0
  cat_graph <- igraph::graph_from_adjacency_matrix(
    t(pruned_final_weights),
    weighted = TRUE
  )
  igraph::plot.igraph(cat_graph)
}

inspect_posterior_track_cat_mapping <- function(track_map) {
  probs <- select(track_map, -cat, -cat_prob, -contains("rank"))
  correlation <- cor(probs, method = "spearman")
  hist(track_map$cat_prob * 100)
  message(sprintf(
    "Median max prob. per track:\n%.0f",
    median(track_map$cat_prob) * 100
  ))
  message("Genre Category # of tracks:")
  print(table(track_map$cat))
  message(sprintf(
    "Gini Single Categories:\n%.3f",
    DescTools::Gini(table(track_map$cat))
  ))
  message("Plotting correlation of track category probabilities.")
  corrplot::corrplot(
    correlation,
    method = "number",
    diag = FALSE,
    order = "AOE",
    type = "lower"
  )
}


get_example_tracks_for_categories <- function(
  long_cat,
  n_per_cat = 2,
  nosinglecats = FALSE
) {
  if (nosinglecats) {
    long_cat <- filter(long_cat, cat_prob < 1)
  }
  long_cat |>
    filter(!is.na(track.s.previewurl)) |>
    group_by(cat) |>
    arrange(desc(cat_prob), desc(track.s.popularity)) |>
    distinct(track.s.firstartist.name, .keep_all = TRUE) |>
    slice_head(n = n_per_cat) |>
    ungroup() |>
    select(
      id,
      cat,
      cat_prob,
      track.s.title,
      track.s.firstartist.name,
      track.s.title,
      album.s.coverurl,
      track.s.previewurl,
      track.s.popularity,
      album.s.releaseyear
    )
}
