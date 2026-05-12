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
# generate_report("03_metagenres")

# get static metagenre graph for paper
mb_res <- readRDS("models/metagenres/tune_mb_metagenres.rds")
sizes <- mb_res$solutions[["1195"]]$n_songs$n # 25 genres
names(sizes) <- mb_res$solutions[["1195"]]$n_songs$genre
sizes <- c(sizes, "POPULAR MUSIC" = 0.001)

genres_5 <- mb_res$solutions[["12930"]]$n_songs$genre
genres_12 <- mb_res$solutions[["3520"]]$n_songs$genre
genres_25 <- mb_res$solutions[["1485"]]$n_songs$genre
genres_32 <- mb_res$solutions[["1195"]]$n_songs$genre
add12 <- setdiff(genres_12, genres_5)
add25 <- setdiff(genres_25, genres_12)
add32 <- setdiff(genres_32, genres_25)

fills <- c(
  rep("#3e578eff", length(genres_5)),
  rep("#c40d20", length(add12)),
  rep("#139e07ff", length(add25)),
  rep("#979797ff", length(add32))
)
names(fills) <- c(genres_5, add12, add25, add32)
fills <- c(fills, "POPULAR MUSIC" = "#ffffffff")

mb_res$solutions[["1195"]]$metagenre_graph |>
  plot_static_tree_graph(
    sizes_lookup = sizes,
    fill_lookup = fills,
    layout = "vertical",
    horizontal_label_levels = 2,
    spacing_x = 1,
    spacing_y = 150,
    font_size = 14,
    output_file = "reports/paper_v1/figures/supergenre_hierarchy.png"
  )
