rm(list = ls())
gc()
devtools::load_all()

min_n <- 1000
step <- 5
max_n <- 15000
optimal_range <- c(25, 30)

# Tune metagenres ----
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
generate_report("03_metagenres")
beepr::beep()

# get static metagenre graph for paper
# sizes <- mb_res$solutions[["1020"]]$n_songs$n
# names(sizes) <- mb_res$solutions[["1020"]]$n_songs$genre
# sizes <- c(sizes, "POPULAR MUSIC" = 0.001)

# mb_res$solutions[["1020"]]$metagenre_graph |>
#   plot_static_tree_graph(
#     sizes_lookup = sizes,
#     fill_lookup = get_fills_lookup(read_feather_with_lists(
#       "data/filtered_mb_long.feather"
#     )),
#     layout = "vertical",
#     spacing_factor = 0.4,
#     output_file = "reports/paper_v1/figures/supergenre_hierarchy.png"
#   )
