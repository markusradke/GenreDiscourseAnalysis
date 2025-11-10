rm(list = ls())
devtools::load_all()

min_n <- 1000
step <- 5
max_n <- 15000
optimal_range <- c(25, 30)

# tune metagenres ----
message("Tuning metagenres...")
tune_mb <- tune_tree_folding(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  readRDS("models/trees/MusicBrainz_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  min_n_grid_max = max_n,
  optimal_solution_range_n_metagenres = optimal_range
)
save_tuning(tune_mb, "mb")

tune_s <- tune_tree_folding(
  read_feather_with_lists("data/filtered_s_long.feather"),
  readRDS("models/trees/Spotify_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  min_n_grid_max = max_n,
  optimal_solution_range_n_metagenres = optimal_range
)
save_tuning(tune_s, "s")

# map genres for solution in 25-30 and 10-15 ranges ----
message("Mapping metagenres for solution ranges...")
map_genres_solution_range <- function(long, tune, n_range) {
  suggested_min_n <- tune$ginis |>
    dplyr::filter(n_metagenres >= n_range[1], n_metagenres <= n_range[2]) |>
    dplyr::arrange(weighted_gini, dplyr::desc(min_n)) |>
    dplyr::slice(1) |>
    dplyr::pull(min_n)
  suggested <- tune$solutions[[as.character(suggested_min_n)]]
  map_metagenres(long, suggested$mapping)
}

mb_25_30 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  c(25, 30)
)
save_feather_with_lists(
  mb_25_30,
  "models/metagenres/mb_metagenres_25_30.feather"
)

s_25_30 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  c(25, 30)
)
save_feather_with_lists(
  s_25_30,
  "models/metagenres/s_metagenres_25_30.feather"
)

mb_10_15 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  c(10, 15)
)
save_feather_with_lists(
  mb_10_15,
  "models/metagenres/mb_metagenres_10_15.feather"
)

s_10_15 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  c(10, 15)
)
save_feather_with_lists(
  s_10_15,
  "models/metagenres/s_metagenres_10_15.feather"
)

generate_report("03_metagenres")
generate_report("04_metagenre_examples")
