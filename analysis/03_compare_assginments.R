rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
devtools::load_all()

# SETUP ----
MIN_ARTISTS <- 10

# LOAD DATA ----
holdout <- fread("data/splits/holdout.csv")
train <- fread("data/splits/train.csv")
ab <- fread("data/long_data/ab_long.csv")
dc <- fread("data/long_data/dc_long.csv")
dz <- fread("data/long_data/dz_long.csv")

dag_fold1 <- readr::read_csv("data/splits/dag_fold1.csv")
dag_fold2 <- readr::read_csv("data/splits/dag_fold2.csv")

P_holdout <- readr::read_csv("data/normalized_voting_matrices/P_holdout.csv") |>
  as.matrix()
P_train <- readr::read_csv("data/normalized_voting_matrices/P_train.csv") |>
  as.matrix()
cat_states <- readRDS("models/dag/cat_states.rds")

# FUNCTIONS ----
ab_filtered <- ab # same genres cause probabities are all non-zero
dc_tags <- get_common_tags(
  dc,
  dag_fold1$track.s.id,
  dag_fold2$track.s.id,
  min_artists = MIN_ARTISTS
)
dc_filtered <- dc |>
  filter(tag_name %in% dc_tags)
dz_tags <- get_common_tags(
  dz,
  dag_fold1$track.s.id,
  dag_fold2$track.s.id,
  min_artists = MIN_ARTISTS
)
dz_filtered <- dz |>
  filter(tag_name %in% dz_tags)

# # of genres
ab_filtered$tag_name |> unique() |> length()
dc_filtered$tag_name |> unique() |> length()
(chosen_k <- dz_filtered$tag_name |> unique() |> length())


# INSPECT MB SOLUTION FOR MAX # OF GENRES ----
G <- ncol(P_holdout)
final_genres <- cat_states$final_genres[[G - chosen_k + 1]]
k_weights <- cat_states$weights[[G - chosen_k + 1]][
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
(k_sizes <- cat_states$sizes[[G - chosen_k + 1]][final_genres])

export_graph_for_gephi_import(
  k_graph,
  sprintf("mb_k%d", chosen_k)
)


# ANALYZE ASSOCIATION WITH EXTERNAL TAXONOMIES ----
# SPEARMAN CORRELATION BETWEEN MB AND AB PROBABILITIES
plot_mb_ab_correlation(holdout, ab_filtered, P_holdout, cat_states)
ggsave(
  "models/dag/spearman_mb_ab.png",
  width = 8,
  height = 6,
  dpi = 600
)

# MEAN PROB OF MUSICMAP FOR DC CATEGORIES ----
plot_mean_prob_mb_for_cat(
  holdout,
  dc_filtered,
  P_holdout,
  cat_states,
  taxonomy_label = "Discogs",
  right_margin_pt = 10
)
ggsave(
  "models/dag/mean_probabilities_mb_dc.png",
  width = 8,
  height = 6,
  dpi = 600
)


# MEAN PROB OF MUSICMAP FOR DZ CATEGORIES ----
plot_mean_prob_mb_for_cat(
  holdout,
  dz_filtered,
  P_holdout,
  cat_states,
  taxonomy_label = "Deezer",
  right_margin_pt = 40
)
ggsave(
  "models/dag/mean_probabilities_mb_dz.png",
  width = 12,
  height = 10,
  dpi = 600
)

# Prepare data for training and holdout ----
train_probs <- get_track_category_probabilities(P_train, cat_states, chosen_k)
holdout_probs <- get_track_category_probabilities(
  P_holdout,
  cat_states,
  chosen_k
)

train_labels <- add_track_map_to_long(train, train_probs) |>
  as.data.frame() |>
  select(track.s.id, cat, cat_prob, all_of(final_genres)) |>
  distinct(track.s.id, .keep_all = TRUE)
holdout_labels <- add_track_map_to_long(holdout, holdout_probs) |>
  as.data.frame() |>
  select(track.s.id, cat, cat_prob, all_of(final_genres)) |>
  distinct(track.s.id, .keep_all = TRUE)
