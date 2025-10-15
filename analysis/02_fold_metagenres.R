rm(list = ls())

min_n <- 1000
step <- 50
optimal_range <- c(20, 30)

tune_mb <- tune_tree_folding(
  read_feather_with_lists("models/initial_genres_mb.feather"),
  readRDS("models/MusicBrainz_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  optimal_solution_range_n_metagenres = optimal_range
)
saveRDS(tune_mb, "models/tune_mb_metagenres.rds")

tune_dc <- tune_tree_folding(
  read_feather_with_lists("models/initial_genres_dc.feather"),
  readRDS("models/Discogs_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  optimal_solution_range_n_metagenres = optimal_range
)
saveRDS(tune_dc, "models/tune_dc_metagenres.rds")

tune_s <- tune_tree_folding(
  read_feather_with_lists("models/initial_genres_s.feather"),
  readRDS("models/Spotify_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  optimal_solution_range_n_metagenres = optimal_range
)
saveRDS(tune_s, "models/tune_s_metagenres.rds")
