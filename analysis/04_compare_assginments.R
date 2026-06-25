rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
devtools::load_all()

# SETUP ----
MIN_ARTISTS <- 10

# LOAD DATA ----
mb <- readRDS("data/mb_long_artist_treshholded.rds")
ab <- fread("data/filtered_ab_long.csv")
dc <- fread("data/filtered_dc_long.csv")
dz <- fread("data/filtered_dz_long.csv")

fold_1_artists <- readRDS("data/fold1.rds") |>
  distinct(track.s.firstartist.id) |>
  pull(track.s.firstartist.id)
fold_2_artists <- readRDS("data/fold2.rds") |>
  distinct(track.s.firstartist.id) |>
  pull(track.s.firstartist.id)

Pfull <- readr::read_csv("data/Pfull.csv") |> as.matrix()
cat_states_full <- readRDS("models/dag/cat_states_full.rds")

# FUNCTIONS ----

filter_min_artists_long <- function(
  long_df,
  min_artists,
  fold_1_artists,
  fold_2_artists
) {
  tags_fold_1 <- long_df |>
    filter(track.s.firstartist.id %in% fold_1_artists) |>
    group_by(tag_name) |>
    summarize(n_artists = n_distinct(track.s.firstartist.id)) |>
    arrange(n_artists) |>
    filter(n_artists >= MIN_ARTISTS)
  tags_fold_2 <- long_df |>
    filter(track.s.firstartist.id %in% fold_2_artists) |>
    group_by(tag_name) |>
    summarize(n_artists = n_distinct(track.s.firstartist.id)) |>
    arrange(n_artists) |>
    filter(n_artists >= MIN_ARTISTS)
  common_tags <- intersect(tags_fold_1$tag_name, tags_fold_2$tag_name)
  long_df |> filter(tag_name %in% common_tags)
}


# DETERMINE # OF GENRES ----
ab_filtered <- ab
dc_filtered <- filter_min_artists_long(
  dc,
  MIN_ARTISTS,
  fold_1_artists,
  fold_2_artists
)
dz_filtered <- filter_min_artists_long(
  dz,
  MIN_ARTISTS,
  fold_1_artists,
  fold_2_artists
)

# # of genres
ab_filtered$tag_name |> unique() |> length() # 7
dc_filtered$tag_name |> unique() |> length() # 12
dz_filtered$tag_name |> unique() |> length() # 34


# INSPECT MB SOLUTION FOR MAX # OF GENRES ----
chosen_k <- 34
G <- ncol(Pfull)
final_genres <- tail(cat_states_full$processing_order, chosen_k)
k_weights <- cat_states_full$weights[[G - chosen_k + 1]][
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
(k_sizes <- cat_states_full$sizes[[G - chosen_k + 1]][final_genres])

export_graph_for_gephi_import(
  k_graph,
  sprintf("mb_k%d", chosen_k)
)


# ANALYZE ASSOCIATION WITH EXTERNAL TAXONOMIES ----
# SPEARMAN CORRELATION BETWEEN MB AND AB PROBABILITIES
plot_mb_ab_correlation(mb, ab_filtered, Pfull, cat_states_full)
ggsave(
  "models/dag/spearman_mb_ab.png",
  width = 8,
  height = 6,
  dpi = 600
)

# MEAN PROB OF MUSICMAP FOR DC CATEGORIES ----
plot_mean_prob_mb_for_cat(
  mb,
  dc_filtered,
  Pfull,
  cat_states_full,
  taxonomy_label = "Discogs"
)
ggsave(
  "models/dag/mean_probabilities_mb_dc.png",
  width = 8,
  height = 6,
  dpi = 600
)


# MEAN PROB OF MUSICMAP FOR DZ CATEGORIES ----
plot_mean_prob_mb_for_cat(
  mb,
  dz_filtered,
  Pfull,
  cat_states_full,
  taxonomy_label = "Deezer",
  right_margin_pt = 40
)
ggsave(
  "models/dag/mean_probabilities_mb_dz.png",
  width = 12,
  height = 10,
  dpi = 600
)
