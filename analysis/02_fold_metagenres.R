rm(list = ls())
devtools::load_all()

min_n <- 1000
step <- 100
max_n <- 1300
optimal_range <- c(40, 50)

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

generate_report("03_ metagenres")
