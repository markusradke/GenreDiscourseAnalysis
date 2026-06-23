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


# Build mapping ----
P1 <- get_normalized_wide_vote_matrix(fold_1_filtered)
P2 <- get_normalized_wide_vote_matrix(fold_2_filtered)
Pval <- get_normalized_wide_vote_matrix(validation_filtered)
Pfull <- get_normalized_wide_vote_matrix(full_filtered)

dag1 <- get_tag_dag_from_normalized_votes(P1)
dag2 <- get_tag_dag_from_normalized_votes(P2)
dag_full <- get_tag_dag_from_normalized_votes(Pfull)


# cat_states_1_order1 <- get_genre_categories_from_graph(dag1$graph, P1)
# saveRDS(cat_states_1_order1, "models/dag/cat_states_1_order1.rds")
cat_states_1_order1 <- readRDS("models/dag/cat_states_1_order1.rds")
# cat_states_2_order1 <- get_genre_categories_from_graph(
#   dag2$graph,
#   P2,
#   processing_order = cat_states_1_order1$processing_order
# )
# saveRDS(cat_states_2_order1, "models/dag/cat_states_2_order1.rds")
cat_states_2_order1 <- readRDS("models/dag/cat_states_2_order1.rds")

k <- 25
final_genres <- tail(cat_states_1_order1$processing_order, k)
mapping11 <- cat_states_1_order1$mapping[[275 - k + 1]]
mapping21 <- cat_states_2_order1$mapping[[275 - k + 1]]
rownames(mapping11) <- colnames(mapping11)
mapping11 <- mapping11[final_genres, ]
rownames(mapping21) <- colnames(mapping21)
mapping21 <- mapping21[final_genres, ]
flat_differences <- c(mapping11 - mapping21)
t.test(flat_differences) # im Mittel unterscheiden sich die Gewichte zu den Kategorien nicht
row_jsds <- row_jsd(mapping11, mapping21)
names(row_jsds) <- rownames(mapping11)
row_jsds |> sort(decreasing = TRUE)
mean(row_jsds)

track_map_1_order1 <- get_track_category_probabilities(
  Pval,
  cat_states_1_order1,
  k
)
track_map_2_order1 <- get_track_category_probabilities(
  Pval,
  cat_states_2_order1,
  k
)
fac1 <- factor(track_map_1_order1$cat)
fac2 <- factor(track_map_2_order1$cat, levels = levels(fac1))
yardstick::f_meas_vec(fac1, fac2, estimator = "macro")


cat_states_2_order2 <- get_genre_categories_from_graph(dag2$graph, P2)
saveRDS(cat_states_2_order2, "models/dag/cat_states_2_order2.rds")
cat_states_1_order2 <- get_genre_categories_from_graph(
  dag1$graph,
  P1,
  processing_order = cat_states_2_order2$processing_order
)
saveRDS(cat_states_1_order2, "models/dag/cat_states_1_order2.rds")

track_map_1_order2 <- get_track_category_probabilities(
  Pval,
  cat_states_1_order2,
  75
)
track_map_2_order2 <- get_track_category_probabilities(
  Pval,
  cat_states_2_order2,
  75
)
# count and pad
fac1 <- factor(track_map_1_order2$cat)
fac2 <- factor(track_map_2_order2$cat, levels = levels(fac1))
yardstick::f_meas_vec(fac1, fac2, estimator = "macro")


cat_states_full <- get_genre_categories_from_graph(dag_full$graph, Pfull)
cat_states_full_wout_weights <- get_genre_categories_from_graph(
  dag_full$graph,
  Pfull
)
saveRDS(cat_states_full, "models/dag/cat_states_full.rds")


# Inspection ----

# content full data set
plot_gini_genre_categories(cat_states_full_wout_weights$ginis, xlim_max = 30)
chosen_k <- 16
inspect_k_categories_solution(cat_states_full, chosen_k, edgevis_thresh = 0.25)
inspect_k_categories_solution(
  cat_states_full_wout_weights,
  chosen_k,
  edgevis_thresh = 0.25
)

track_map <- get_track_category_probabilities(Pfull, cat_states_full, chosen_k)
# inspect track mapping
# track_map <- get_single_genres(track_map, top_cat = 2)
# inspect_posterior_track_cat_mapping(track_map)

mb_cat <- add_single_genre_categories_to_tracks(long, track_map)
get_example_tracks_for_categories(mb_cat, nosinglecats = FALSE, n = 5) |> View()
