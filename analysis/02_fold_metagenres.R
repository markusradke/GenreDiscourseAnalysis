rm(list = ls())
devtools::load_all()

min_n <- 1000
step <- 20
optimal_range <- c(20, 30)

tune_mb <- tune_tree_folding(
  read_feather_with_lists("models/trees/initial_genres_mb.feather"),
  readRDS("models/trees/MusicBrainz_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  optimal_solution_range_n_metagenres = optimal_range
)
saveRDS(tune_mb, "models/metagenres/tune_mb_metagenres.rds")
saveRDS(
  tune_mb$suggested_solution,
  "models/metagenres/metagenres_mb_suggested_solution.rds"
)
saveRDS(tune$plot, "models/metagenres/metagenres_mb_gini_plot.rds")

tune_dc <- tune_tree_folding(
  read_feather_with_lists("models/trees/initial_genres_dc.feather"),
  readRDS("models/trees/Discogs_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  optimal_solution_range_n_metagenres = optimal_range
)
saveRDS(tune_dc, "models/metagenres/tune_dc_metagenres.rds")
saveRDS(
  tune_dc$suggested_solution,
  "models/metagenres/metagenres_dc_suggested_solution.rds"
)
saveRDS(tune$plot, "models/metagenres/metagenres_dc_gini_plot.rds")

tune_s <- tune_tree_folding(
  read_feather_with_lists("models/trees/initial_genres_s.feather"),
  readRDS("models/trees/Spotify_graph.rds"),
  min_n_grid_min = min_n,
  min_n_grid_step = step,
  optimal_solution_range_n_metagenres = optimal_range
)
saveRDS(tune_s, "models/metagenres/tune_s_metagenres.rds")
saveRDS(
  tune_s$suggested_solution,
  "models/metagenres/metagenres_s_suggested_solution.rds"
)
saveRDS(tune$plot, "models/metagenres/metagenres_s_gini_plot.rds")
