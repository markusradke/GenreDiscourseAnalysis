rm(list = ls())
gc()
devtools::load_all()
library(dplyr)
library(readr)
library(ggplot2)
library(igraph)

RANDOM_STATE <- 42

mb <- read_csv("data/filtered_mb_long.csv")

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

common_tags <- intersect(unique(fold_1$tag_name), unique(fold_2$tag_name))
fold_1_filtered <- fold_1 |> filter(tag_name %in% common_tags)
fold_2_filtered <- fold_2 |> filter(tag_name %in% common_tags)


# just for inspection
hist(fold_1_filtered$artist.s.popularity)
hist(fold_2_filtered$artist.s.popularity)
hist(fold_1_filtered$album.s.releaseyear)
hist(fold_2_filtered$album.s.releaseyear)

# Test
message("\nBUILDING MUSICBRAINZ GENRE TREE")
fold_1_full_tree <- build_genre_tree(
  fold_1_filtered,
  "mb_fold_1",
  min_cooc = 1,
  vote_weighted = TRUE
)
fold_2_full_tree <- build_genre_tree(
  fold_2_filtered,
  "mb_fold_2",
  min_cooc = 1,
  vote_weighted = TRUE
)
generate_report("interactive_COMGET_browser_mb_fold1")
generate_report("interactive_COMGET_browser_mb_fold2")

min_n <- 1000
step <- 5
max_n <- 15000
optimal_range <- c(20, 30)

# Tune metagenres ----
message("Tuning metagenres...")
tune_mb_fold_1 <- tune_tree_folding(
  fold_1_filtered,
  readRDS("models/trees/mb_fold_1_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  min_n_grid_max = max_n,
  optimal_solution_range_n_metagenres = optimal_range
)
save_tuning(tune_mb_fold_1, "mb_fold_1")

tune_mb_fold_2 <- tune_tree_folding(
  fold_2_filtered,
  readRDS("models/trees/mb_fold_2_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  min_n_grid_max = max_n,
  optimal_solution_range_n_metagenres = optimal_range
)
save_tuning(tune_mb_fold_2, "mb_fold_2")

tune_mb_fold_1 <- readRDS("models/metagenres/tune_mb_fold_1_metagenres.rds")
tune_mb_fold_2 <- readRDS("models/metagenres/tune_mb_fold_2_metagenres.rds")

ginis_fold1 <- tune_mb_fold_1$ginis |> mutate(fold = "Fold 1")
ginis_fold2 <- tune_mb_fold_2$ginis |> mutate(fold = "Fold 2")
ginis_combined <- bind_rows(ginis_fold1, ginis_fold2)

ggplot(ginis_combined, aes(x = n_metagenres, y = weighted_gini, color = fold)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Gini Coefficient for Different min_n Values",
    x = "min_n",
    y = "Gini Coefficient"
  ) +
  theme_minimal()

n_categories <- 8
min_n_fold1 <- ginis_fold1 |>
  filter(n_metagenres == n_categories) |>
  slice_max(min_n) |>
  pull(min_n)
min_n_fold2 <- ginis_fold2 |>
  filter(n_metagenres == n_categories) |>
  slice_max(min_n) |>
  pull(min_n)
solution_fold_1 <- tune_mb_fold_1$solutions[[as.character(min_n_fold1)]]
solution_fold_2 <- tune_mb_fold_2$solutions[[as.character(min_n_fold2)]]

solution_fold_1$n_songs |> pull(genre)
solution_fold_2$n_songs |> pull(genre)

igraph::plot.igraph(solution_fold_1$metagenre_graph)
igraph::plot.igraph(solution_fold_2$metagenre_graph)
# mb_support <- get_support_for_tree_edges(mb_res$final_tree, mb)
# mb_artists <- dplyr::distinct(mb, trackartists.s.id, tag_name, .keep_all = TRUE)
# mb_artist_support <- get_support_for_tree_edges(mb_res$final_tree, mb_artists)

cross_compare_mappings <- function(
  tune1,
  tune2,
  fold1_data,
  fold2_data,
  n_categories
) {
  .compare_mappings <- function(mapping_a, mapping_b) {
    comparison <- mapping_a |>
      select(track.s.id, metagenre_from_fold_a = metagenre) |>
      left_join(
        mapping_b |> select(track.s.id, metagenre_from_fold_b = metagenre),
        by = "track.s.id"
      ) |>
      mutate(same = metagenre_from_fold_a == metagenre_from_fold_b)

    proportion_same <- mean(comparison$same) * 100
    message(sprintf(
      "Proportion of tracks with same metagenre assignment: %.2f%%",
      proportion_same
    ))
  }

  mapping1to1 <- map_genres_solution_range(
    fold1_data,
    tune1,
    n_categories,
    1,
    0
  )

  mapping1to2 <- map_genres_solution_range(
    fold1_data,
    tune2,
    n_categories,
    1,
    0
  )

  mapping2to1 <- map_genres_solution_range(
    fold2_data,
    tune1,
    n_categories,
    1,
    0
  )

  mapping2to2 <- map_genres_solution_range(
    fold2_data,
    tune2,
    n_categories,
    1,
    0
  )

  .compare_mappings(mapping1to1, mapping1to2)
  .compare_mappings(mapping2to1, mapping2to2)
}
cross_compare_mappings(
  tune_mb_fold_1,
  tune_mb_fold_2,
  fold_1_filtered,
  fold_2_filtered,
  n_categories
)


# Find matching backbone genre hierarchies between folds
tree_fold_1 <- readRDS("models/trees/mb_fold_1_graph.rds")
tree_fold_2 <- readRDS("models/trees/mb_fold_2_graph.rds")

fold_1_edges <- as_data_frame(tree_fold_1, what = "edges") |>
  select(from, to) |>
  mutate(edge_id = paste(from, to, sep = "_"))
fold_2_edges <- as_data_frame(tree_fold_2, what = "edges") |>
  select(from, to) |>
  mutate(edge_id = paste(from, to, sep = "_"))
matching_edges <- inner_join(fold_1_edges, fold_2_edges, by = "edge_id") |>
  select(from = from.x, to = to.x)

matching_nodes <- union(matching_edges$from, matching_edges$to) |> unique()
length(matching_nodes)
fold_1_node_names <- V(tree_fold_1)$name
length(fold_1_node_names)
lost_nodes <- setdiff(fold_1_node_names, matching_nodes)
length(lost_nodes)
print(lost_nodes)
# Example Acid Jazz
fold_1_edges |> filter(from == "acid jazz") |> select(to)
fold_2_edges |> filter(from == "acid jazz") |> select(to)
fold_1_edges |> filter(from == "neue deutsche welle") |> select(to)
fold_2_edges |> filter(from == "neue deutsche welle") |> select(to)

# Try Spotify Source only ----
only_spotify <- mb |>
  filter(
    source.featuredplaylists | source.recommendations | source.spotifycharts
  )
count(only_spotify, tag_name, sort = TRUE) |> head(100) |> print(n = Inf)

message(sprintf(
  "Number of only Spotify sources tracks: %d",
  nrow(distinct(only_spotify, track.s.id))
))


spotify_sources_tree <- build_genre_tree(
  only_spotify,
  "mb_spotify_sources",
  min_cooc = 1,
  vote_weighted = TRUE
)
generate_report("interactive_COMGET_browser_mb_spotify_sources")

min_n <- 100
step <- 10
max_n <- 5000
optimal_range <- c(10, 20)
tune_mb_spotify_sources <- tune_tree_folding(
  only_spotify,
  readRDS("models/trees/mb_spotify_sources_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  min_n_grid_max = max_n,
  optimal_solution_range_n_metagenres = optimal_range
)
save_tuning(tune_mb_spotify_sources, "mb_spotify_sources")

tune_mb_spotify_sources <- readRDS(
  "models/metagenres/tune_mb_spotify_sources_metagenres.rds"
)
ginis_spot <- tune_mb_spotify_sources$ginis

ggplot(ginis_spot, aes(x = n_metagenres, y = weighted_gini)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Gini Coefficient for Different min_n Values",
    x = "min_n",
    y = "Gini Coefficient"
  ) +
  theme_minimal()

n_categories <- 11
n_categories <- 19
n_categories <- 28
min_n_spot <- ginis_spot |>
  filter(n_metagenres == n_categories) |>
  slice_max(min_n) |>
  pull(min_n)
solution_spot <- tune_mb_spotify_sources$solutions[[as.character(min_n_spot)]]

solution_spot$n_songs |> pull(genre)

igraph::plot.igraph(solution_spot$metagenre_graph)
