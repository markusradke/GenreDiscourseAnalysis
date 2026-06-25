get_normalized_wide_vote_matrix <- function(long_df) {
  # TODO add doc
  wide <- dcast(
    long_df,
    id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  V <- as.matrix(wide[, -1, with = FALSE])
  Vt <- rowSums(V)
  P <- V / Vt # normalize vote counts per track
  return(P)
}


get_tag_dag_from_normalized_votes <- function(P) {
  # TODO add doc

  message("Calculating probabilities and building trees...")
  N <- nrow(P)
  G <- ncol(P)

  # Get marginals, joint and conditional probabilities
  P_g <- colMeans(P)
  eps <- 1e-12
  P_g_smooth <- pmax(P_g, eps) # prevent divide by zero
  joint_raw <- crossprod(P) / N # P(X, Y)
  cond_b_given_a <- sweep(joint_raw, 1, P_g_smooth, FUN = "/") # P(B | A) = P(A, B) / P(B), rows correspond to A
  weights <- cond_b_given_a
  diag(weights) <- 0 # so self-loops

  # get weighted number of indegrees as a measure of globality for a tag
  indegree_weights <- colSums(weights, na.rm = TRUE)
  message(sprintf(
    "Tags with top 20 generality (descending order):\n%s",
    toString(names(indegree_weights |> sort(decreasing = TRUE) |> head(20)))
  ))

  # Build DAG
  edges <- which(outer(indegree_weights, indegree_weights, "<"), arr.ind = TRUE) # allowed pairs
  E <- data.frame(
    from = colnames(P)[edges[, 1]],
    to = colnames(P)[edges[, 2]],
    weight = weights[edges]
  )
  g <- graph_from_data_frame(E, directed = TRUE)
  dangling <- V(g)[degree(g, mode = "out") == 0]$name
  message(sprintf("Nodes without any supergenres: \n%s", toString(dangling)))
  return(list(graph = g, generality = indegree_weights))
}


get_genre_categories_from_graph <- function(g, P, processing_order = NULL) {
  if (!is.null(processing_order)) {
    is_fixed_order <- TRUE
    processing_order <- match(processing_order, colnames(P))
  } else {
    is_fixed_order <- FALSE
  }
  P <- as(P, "sparseMatrix")
  weights <- t(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  weights <- weights[order(colnames(weights)), order(colnames(weights))]

  weights_norm <- sweep(
    weights,
    2,
    colSums(weights),
    FUN = "/"
  )
  weights <- weights_norm
  rownames(weights) <- colnames(weights)

  sizes <- calculate_sizes_from_track_mapping(P)

  mapping <- diag(nrow(weights)) # initialize mapping weights as identity
  colnames(mapping) <- colnames(weights)
  mapping

  G <- ncol(P)
  processed_genres <- c()
  ginis <- c()
  mappings_states <- list()
  weights_states <- list()
  sizes_states <- list()

  current_weights <- weights
  current_mapping <- mapping
  current_track_mapping <- P
  current_sizes <- sizes
  current_gini <- DescTools::Gini(sizes)

  mappings_states[[1]] <- current_mapping
  weights_states[[1]] <- weights
  sizes_states[[1]] <- current_sizes
  ginis[1] <- current_gini

  # process in order of initial sizes
  if (is_fixed_order) {
    next_genre <- processing_order[1]
  } else {
    next_genre <- which.min(sizes)
  }
  i <- 1
  while (length(processed_genres) < G) {
    message(sprintf(
      "Processing genre %d of %d: %s",
      i,
      G,
      colnames(weights)[next_genre]
    ))
    weight_column <- current_weights[, next_genre]
    add_mapping <- weight_column %*% t(current_mapping[next_genre, ])
    current_mapping[next_genre, ] <- 0
    current_mapping <- current_mapping + add_mapping

    add_weights <- weight_column %*% t(current_weights[next_genre, ])
    current_weights[next_genre, ] <- 0
    current_weights <- current_weights + add_weights

    current_track_mapping <- P %*% t(current_mapping)
    colnames(current_track_mapping) <- colnames(P)
    current_sizes <- calculate_sizes_from_track_mapping(current_track_mapping)

    mappings_states[[i + 1]] <- current_mapping
    weights_states[[i + 1]] <- current_weights
    sizes_states[[i + 1]] <- current_sizes
    processed_genres <- c(processed_genres, next_genre)
    current_gini <- DescTools::Gini(current_sizes[-processed_genres])
    ginis[i + 1] <- current_gini
    if (!is_fixed_order) {
      next_genre_name <- which.min(current_sizes[-processed_genres]) |> names()
      next_genre <- which(colnames(P) == next_genre_name)
    } else {
      next_genre <- processing_order[i + 1]
    }
    i <- i + 1
  }
  return(list(
    mappings = mappings_states,
    weights = weights_states,
    sizes = sizes_states,
    ginis = ginis,
    processing_order = colnames(P)[processed_genres]
  ))
}

calculate_sizes_from_track_mapping <- function(track_mapping) {
  message("Calculating track mapping...")
  winners <- apply(track_mapping, 1, which.max)
  winners_names <- colnames(track_mapping)[winners]
  sizes <- table(winners_names)
  missing <- setdiff(colnames(track_mapping), names(sizes))
  missing <- setNames(rep(0, length(missing)), missing)
  sizes <- c(sizes, missing)
  sizes <- sizes[colnames(track_mapping)]
  message("Done.")
  sizes
}

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

get_track_category_probabilities <- function(P, res_list, chosen_k) {
  P <- as(P, "sparseMatrix")
  G <- nrow(res_list$mappings[[1]])
  final_mapping <- res_list$mappings[[G - chosen_k + 1]]
  final_sizes <- res_list$sizes[[G - chosen_k + 1]]
  final_genres <- tail(res_list$processing_order, chosen_k)
  prob_track_map <- P %*% t(final_mapping)
  colnames(prob_track_map) <- colnames(final_mapping)
  prob_track_map <- prob_track_map[, final_genres]

  cat <- colnames(prob_track_map)[apply(prob_track_map, 1, which.max)]
  cat_prob <- apply(prob_track_map, 1, max)
  prob_track_map <- as.data.frame(as.matrix(prob_track_map))
  prob_track_map$cat <- cat
  prob_track_map$cat_prob <- cat_prob
  prob_track_map
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

add_single_genre_categories_to_tracks <- function(long_df, track_map) {
  wide <- dcast(
    long_df,
    id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  mapping <- cbind(id = wide$id, track_map)
  left_join(long_df, mapping, by = "id")
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
