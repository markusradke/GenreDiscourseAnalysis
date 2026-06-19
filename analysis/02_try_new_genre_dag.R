rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
library(igraph)
devtools::load_all()

RANDOM_STATE <- 42

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

  # Get marginals, joint and conditional probabilities and compute log odds lift weights
  P_g <- colMeans(P)
  eps <- 1e-12
  P_g_smooth <- pmax(P_g, eps) # prevent divide by zero
  joint_raw <- crossprod(P) / N # P(X, Y)
  cond_b_given_a <- sweep(joint_raw, 1, P_g_smooth, FUN = "/") # P(B | A) = P(A, B) / P(B), rows correspond to A
  weights <- cond_b_given_a
  diag(weights) <- 0 # so self-loops

  # get weighted number of indegrees as a measure of how globality for a tag
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


get_genre_categories_from_graph <- function(g, P) {
  weights <- t(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  weights <- weights[order(colnames(weights)), order(colnames(weights))]

  # mutliply weights towards supergenres (cols) with inverse indegree (+ 1 to avoid dividing by zero)
  n_nonzero_indegrees <- igraph::as_data_frame(g, what = ("vertices")) |>
    left_join(
      igraph::as_data_frame(g, what = c("edges")) |>
        filter(weight > 0) |>
        count(to),
      by = join_by("name" == "to")
    ) |>
    mutate(nonzero_indegree = ifelse(is.na(n), 1, n + 1))
  graph_weights <- n_nonzero_indegrees$nonzero_indegree
  names(graph_weights) <- n_nonzero_indegrees$name
  graph_weights <- graph_weights[colnames(weights)]

  weights_weighted <- sweep(weights, 1, (1 / graph_weights), FUN = "*")
  # normalize outgoing edges to sum to 1 (rows)
  weights_norm <- sweep(
    weights_weighted,
    2,
    colSums(weights_weighted),
    FUN = "/"
  )
  weights <- weights_norm
  rownames(weights) <- colnames(weights)

  winners <- apply(P, 1, which.max)
  winners_names <- colnames(P)[winners]
  sizes <- table(winners_names)
  missing <- setdiff(colnames(P), names(sizes))
  missing <- setNames(rep(0, length(missing)), missing)
  sizes <- c(sizes, missing)

  # process in order of initial sizes
  order <- names(sizes |> sort(decreasing = FALSE))
  order <- setNames(seq(ncol(P)), colnames(P))[order]
  # TODO should be updated after each repetition

  mapping <- diag(nrow(weights)) # initialize mapping weights as identity
  colnames(mapping) <- colnames(weights)
  mapping

  ginis <- c()
  mappings_states <- list()
  weights_states <- list()
  sizes_states <- list()

  current_gini <- DescTools::Gini(sizes[sizes > 0])
  current_weights <- weights
  current_mapping <- mapping
  current_sizes <- sizes

  mappings_states[[1]] <- current_mapping
  weights_states[[1]] <- weights
  sizes_states[[1]] <- current_sizes
  ginis[1] <- current_gini

  i <- 1
  for (genre in order) {
    message(sprintf(
      "Processing genre %d of %d: %s",
      i,
      length(order),
      colnames(weights)[genre]
    ))
    weight_column <- current_weights[, genre]
    add_mapping <- weight_column %*% t(current_mapping[genre, ])
    current_mapping[genre, ] <- 0
    current_mapping <- current_mapping + add_mapping

    add_weights <- weight_column %*% t(current_weights[genre, ])
    current_weights[genre, ] <- 0
    current_weights <- current_weights + add_weights

    current_sizes <- sizes %*% t(current_mapping) |> as.vector()
    names(current_sizes) <- colnames(current_mapping)
    current_gini <- DescTools::Gini(current_sizes[current_sizes > 0])
    mappings_states[[i + 1]] <- current_mapping
    weights_states[[i + 1]] <- current_weights
    ginis[i + 1] <- current_gini
    sizes_states[[i + 1]] <- current_sizes
    i <- i + 1
  }
  return(list(
    mappings = mappings_states,
    weights = weights_states,
    sizes = sizes_states,
    ginis = ginis
  ))
}

plot_gini_genre_categories <- function(ginis) {
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
    xlim(0, 50)
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

get_track_category_probabilities_and_ranks <- function(P, res_list, chosen_k) {
  message(
    "Calculating track category probabilities and ranks; this might take up to one minute..."
  )
  G <- nrow(res_list$mappings[[1]])
  final_mapping <- res_list$mappings[[G - chosen_k + 1]]
  final_sizes <- res_list$sizes[[G - chosen_k + 1]]
  final_genres <- names(final_sizes[final_sizes > 0])
  prob_track_map <- P %*% t(final_mapping)
  colnames(prob_track_map) <- colnames(final_mapping)
  prob_track_map <- prob_track_map[, final_genres]
  probs <- as.data.frame(prob_track_map)
  rankings <- probs |>
    as.matrix() |>
    apply(1, rank) |>
    t()
  colnames(rankings) <- paste0("rank_", colnames(rankings))
  rankings <- as.data.frame(chosen_k - rankings + 1)
  res <- cbind(probs, rankings)
  message("Done.")
  res
}

# get_single_genres <- function(track_map, top_cat = 2) {
#   sizes <- track_map |> select(-contains("rank")) |> summarize_all(sum)
#   sizes <- t(sizes)[, 1]
#   probs <- track_map |>
#     select(-contains("rank")) |>
#     mutate(id = seq(nrow(track_map))) |>
#     select(id, everything())
#   probs_long <- tidyr::pivot_longer(
#     probs,
#     2:ncol(probs),
#     names_to = "cat",
#     values_to = "prob"
#   )
#   ranks <- track_map |>
#     select(contains("rank")) |>
#     rename_all(~ stringr::str_sub(., start = 6)) |>
#     mutate(id = seq(nrow(track_map))) |>
#     select(id, everything())
#   ranks_long <- tidyr::pivot_longer(
#     ranks,
#     2:ncol(ranks),
#     names_to = "cat",
#     values_to = "rank"
#   )
#   ranks_probs_long <- inner_join(probs_long, ranks_long, by = c("id", "cat"))

#   ranks_probs_long |>
#     filter(rank <= top_cat) |>
#     mutate(size = sizes[cat]) |>
#     group_by(id) |>
#     slice_min(size, n = 1)
# }

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


# MAIN ----
mb <- fread("data/filtered_mb_long.csv") |> rename(id = track.s.id)


# artist grouped split stratified by popularity and release year
artist_info <- mb |>
  group_by(track.s.firstartist.id) |>
  summarize(
    median_release_year = median(album.s.releaseyear, na.rm = TRUE),
    artist_popularity = median(artist.s.popularity, na.rm = TRUE) # should be same for all tracks of an artist
  ) |>
  mutate(
    popularity_bin = cut(
      artist_popularity,
      breaks = quantile(
        artist_popularity,
        probs = c(0, 0.33, 0.66, 1),
        na.rm = TRUE
      ),
      labels = FALSE
    ),
    release_year_bin = cut(
      median_release_year,
      quantile(
        median_release_year,
        probs = c(0, 0.33, 0.66, 1),
        na.rm = TRUE
      ),
      labels = FALSE
    ),
    strata = paste(popularity_bin, release_year_bin, sep = "_")
  )

## make stratified artist split
set.seed(RANDOM_STATE)
split <- rsample::initial_split(
  artist_info,
  prop = 0.5,
  strata = "strata"
)
fold_1_artists <- rsample::training(split) |> select(track.s.firstartist.id)
fold_2_artists <- rsample::testing(split) |> select(track.s.firstartist.id)

fold_1 <- mb |>
  filter(track.s.firstartist.id %in% fold_1_artists$track.s.firstartist.id) |>
  mutate(fold = 1)
fold_2 <- mb |>
  filter(track.s.firstartist.id %in% fold_2_artists$track.s.firstartist.id) |>
  mutate(fold = 2)

fold1_artist_thresh <- fold_1 |>
  filter_tags_by_artist_occurrences(n_min_artists = 10)
fold2_artist_thresh <- fold_2 |>
  filter_tags_by_artist_occurrences(n_min_artists = 10)

common_tags <- intersect(
  unique(fold1_artist_thresh$tag_name),
  unique(fold2_artist_thresh$tag_name)
)

fold_1_filtered <- fold_1 |>
  filter(tag_name %in% common_tags)
fold_2_filtered <- fold_2 |>
  filter(tag_name %in% common_tags)


# try Spotify only
# mb <- mb |>
#   filter(
#     source.featuredplaylists | source.recommendations | source.spotifycharts
#   )
# mb <- filter_tags_by_artist_occurrences(mb, n_min_artists = 10)

long <- mb |> filter_tags_by_artist_occurrences(n_min_artists = 10)
long <- fold_1_filtered
long <- fold_2_filtered

P <- get_normalized_wide_vote_matrix(long)
dag <- get_tag_dag_from_normalized_votes(P)
cat_states <- get_genre_categories_from_graph(dag$graph, P)

# erste 9 der generalisierenden sind gleich

plot_gini_genre_categories(cat_states$ginis)
chosen_k <- 10
inspect_k_categories_solution(cat_states, chosen_k, edgevis_thresh = 0.15)

track_map <- get_track_category_probabilities_and_ranks(P, cat_states, chosen_k)
# inspect track mapping
# track_map <- get_single_genres(track_map, top_cat = 2)
# inspect_posterior_track_cat_mapping(track_map)

mb_cat <- add_single_genre_categories_to_tracks(long, track_map)
get_example_tracks_for_categories(mb_cat, nosinglecats = TRUE, n = 5) |> View()
