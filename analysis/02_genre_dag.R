rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
library(igraph)
library(Matrix)
devtools::load_all()


N_MIN_ARTISTS <- 10
RANDOM_STATE <- 42


# MAIN ----
# Read data and build splits ----
mb <- fread("data/long_data/mb_long.csv")
initial_split_ids <- split_artist_level_popularity_release_year(
  mb,
  test_size = 0.8,
  seed = RANDOM_STATE
)
mb_holdout <- mb |>
  filter(track.s.id %in% initial_split_ids$test_tracks)
mb_train <- mb |>
  filter(track.s.id %in% initial_split_ids$train_tracks)
mb_build_dag <- mb |>
  filter(track.s.id %in% initial_split_ids$train_tracks)
dag_validation_split_ids <- split_artist_level_popularity_release_year(
  mb_train,
  test_size = 0.5,
  seed = RANDOM_STATE
)
fold_1 <- mb_train |>
  filter(track.s.id %in% dag_validation_split_ids$train_tracks)
fold_2 <- mb_train |>
  filter(track.s.id %in% dag_validation_split_ids$test_tracks)


common_tags <- get_common_tags(
  mb_train,
  fold_1$track.s.id,
  fold_2$track.s.id,
  min_artists = N_MIN_ARTISTS
)

fold_1_filtered <- fold_1 |>
  filter(tag_name %in% common_tags)
fold_2_filtered <- fold_2 |>
  filter(tag_name %in% common_tags)
holdout_filtered <- mb_holdout |>
  filter(tag_name %in% common_tags)
train_filtered <- bind_rows(fold_1_filtered, fold_2_filtered)

readr::write_csv(fold_1_filtered, "data/splits/dag_fold1.csv")
readr::write_csv(fold_2_filtered, "data/splits/dag_fold2.csv")
readr::write_csv(holdout_filtered, "data/splits/holdout.csv")
readr::write_csv(train_filtered, "data/splits/train.csv")

# Build mapping ----
P1 <- get_normalized_wide_vote_matrix(fold_1_filtered)
P2 <- get_normalized_wide_vote_matrix(fold_2_filtered)
P_holdout <- get_normalized_wide_vote_matrix(holdout_filtered)
P_train <- get_normalized_wide_vote_matrix(train_filtered)

readr::write_csv(
  as.data.frame(P_holdout),
  "data/normalized_voting_matrices/P_holdout.csv"
)
readr::write_csv(
  as.data.frame(P_train),
  "data/normalized_voting_matrices/P_train.csv"
)

dag1 <- get_tag_dag_from_normalized_votes(P1)
dag2 <- get_tag_dag_from_normalized_votes(P2)
dag_train <- get_tag_dag_from_normalized_votes(P_train)


# get models for robustness check ----
cat_states_11 <- get_genre_categories_from_graph(dag1$graph, P1)
saveRDS(cat_states_11, "models/dag/cat_states_fold1_order1.rds")
cat_states_21 <- get_genre_categories_from_graph(
  dag2$graph,
  P2,
  processing_order = cat_states_11$processing_order
)
saveRDS(cat_states_21, "models/dag/cat_states_fold2_order1.rds")
cat_states_22 <- get_genre_categories_from_graph(dag2$graph, P2)
saveRDS(cat_states_22, "models/dag/cat_states_fold2_order2.rds")
cat_states_12 <- get_genre_categories_from_graph(
  dag1$graph,
  P1,
  processing_order = cat_states_22$processing_order
)
saveRDS(cat_states_12, "models/dag/cat_states_fold1_order2.rds")

# get full training set model ----
cat_states <- get_genre_categories_from_graph(dag_train$graph, P_train)
saveRDS(cat_states, "models/dag/cat_states.rds")

# BUILD DAG FOR SPOTIFY ----
s_long <- fread("data/long_data/s_long.csv")
s_tags <- get_common_tags(
  s_long,
  dag_validation_split_ids$train_tracks,
  dag_validation_split_ids$test_tracks,
  min_artists = N_MIN_ARTISTS
)
s_train <- s_long |>
  filter(track.s.id %in% initial_split_ids$train_tracks) |>
  filter(tag_name %in% s_tags)
# give me the columns of P_s_train where the entries are only ones and zeros, i.e. no fractional values
P_s_train <- get_normalized_wide_vote_matrix(s_train)
binary_columns <- colnames(P_s_train)[apply(P_s_train, 2, function(x) {
  all(x %in% c(0, 1))
})]
# remove those columns from the training data: These genres do not cooccur with any other genres, so they are not useful for building a DAG
s_train <- s_train |>
  filter(!tag_name %in% binary_columns)
P_s_train <- get_normalized_wide_vote_matrix(s_train)

dag_s_train <- get_tag_dag_from_normalized_votes(P_s_train)

cat_states_s <- get_genre_categories_from_graph(
  dag_s_train$graph,
  P_s_train,
)
saveRDS(cat_states_s, "models/dag/cat_states_spotify.rds")
beepr::beep(3)
plot_gini_genre_categories(cat_states_s, 100)
chosen_k <- 28 # local minimum of gini index < 30

G <- ncol(P_s_train)
final_genres <- setdiff(
  colnames(P_s_train),
  head(cat_states_s$processing_order, G - chosen_k)
)

k_weights <- cat_states_s$weights[[G - chosen_k + 1]][
  final_genres,
  final_genres
]
# make graph and add sizes as node attributes
k_graph <- igraph::graph_from_adjacency_matrix(
  t(k_weights),
  mode = "directed",
  weighted = TRUE
)
# add sizes in gephi manually:
(k_sizes <- cat_states_s$sizes[[G - chosen_k + 1]][final_genres])

# inspect
k_graph_filtered <- igraph::delete_edges(
  k_graph,
  E(k_graph)[is.nan(E(k_graph)$weight)]
)
k_graph_filtered <- igraph::delete_edges(
  k_graph_filtered,
  E(k_graph_filtered)[E(k_graph_filtered)$weight <= 0.1]
)
igraph::plot.igraph(k_graph_filtered, vertex.label.cex = 1)


mapping <- get_track_category_probabilities(
  P_s_train,
  cat_states_s,
  chosen_k
)


wide <- dcast(
  s_train,
  track.s.id ~ tag_name,
  value.var = "tag_count",
  fill = 0
)
s_mapping <- cbind(track.s.id = wide$track.s.id, mapping) |>
  left_join(s_train, mapping, by = "track.s.id") |>
  select(track.s.id, cat, cat_prob, tag_name, everything())
readr::write_csv(s_mapping, "data/modeled_features/spotify_genres_mapping.csv")
