rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
library(igraph)
library(Matrix)
devtools::load_all()

RANDOM_STATE <- 42


# MAIN ----
# Read data and build splits ----
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
  prop = 0.33,
  strata = "strata"
)
fold_1_artists <- rsample::training(split) |> select(track.s.firstartist.id)
fold_2_val_artists <- rsample::testing(split) |>
  select(track.s.firstartist.id, strata)

set.seed(RANDOM_STATE)
split <- rsample::initial_split(
  fold_2_val_artists,
  prop = 0.5,
  strata = "strata"
)
fold_2_artists <- rsample::training(split) |> select(track.s.firstartist.id)
validation_artists <- rsample::testing(split) |> select(track.s.firstartist.id)


fold_1 <- mb |>
  filter(track.s.firstartist.id %in% fold_1_artists$track.s.firstartist.id) |>
  mutate(fold = 1)
fold_2 <- mb |>
  filter(track.s.firstartist.id %in% fold_2_artists$track.s.firstartist.id) |>
  mutate(fold = 2)
validation <- mb |>
  filter(
    track.s.firstartist.id %in% validation_artists$track.s.firstartist.id
  ) |>
  mutate(fold = 3)
saveRDS(fold_1, "data/fold1.rds")
saveRDS(fold_2, "data/fold2.rds")


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
validation_filtered <- validation |>
  filter(tag_name %in% common_tags)
full_filtered <- mb |> filter(tag_name %in% common_tags)
saveRDS(full_filtered, "data/mb_long_artist_treshholded.rds")


# Build mapping ----
P1 <- get_normalized_wide_vote_matrix(fold_1_filtered)
P2 <- get_normalized_wide_vote_matrix(fold_2_filtered)
Pval <- get_normalized_wide_vote_matrix(validation_filtered)
readr::write_csv(as.data.frame(Pval), "data/Pval.csv")
Pfull <- get_normalized_wide_vote_matrix(full_filtered)
readr::write_csv(as.data.frame(Pfull), "data/Pfull.csv")

dag1 <- get_tag_dag_from_normalized_votes(P1)
dag2 <- get_tag_dag_from_normalized_votes(P2)
dag_full <- get_tag_dag_from_normalized_votes(Pfull)


# get models for robustness check ----
cat_states_1_order1 <- get_genre_categories_from_graph(dag1$graph, P1)
saveRDS(cat_states_1_order1, "models/dag/cat_states_1_order1.rds")
cat_states_2_order1 <- get_genre_categories_from_graph(
  dag2$graph,
  P2,
  processing_order = cat_states_1_order1$processing_order
)
saveRDS(cat_states_2_order1, "models/dag/cat_states_2_order1.rds")
cat_states_2_order2 <- get_genre_categories_from_graph(dag2$graph, P2)
saveRDS(cat_states_2_order2, "models/dag/cat_states_2_order2.rds")
cat_states_1_order2 <- get_genre_categories_from_graph(
  dag1$graph,
  P1,
  processing_order = cat_states_2_order2$processing_order
)
saveRDS(cat_states_1_order2, "models/dag/cat_states_1_order2.rds")

# get full model ----
cat_states_full <- get_genre_categories_from_graph(dag_full$graph, Pfull)
cat_states_full_wout_weights <- get_genre_categories_from_graph(
  dag_full$graph,
  Pfull
)
saveRDS(cat_states_full, "models/dag/cat_states_full.rds")
