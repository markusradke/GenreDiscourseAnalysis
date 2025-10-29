rm(list = ls())
devtools::load_all()

# Load data ----
mb <- read_feather_with_lists("data/filtered_mb_long.feather")
s <- read_feather_with_lists("data/filtered_s_long.feather")

# Build genre trees ----
message("\nBUILDING MUSICBRAINZ GENRE TREE")
build_genre_tree(mb, "MusicBrainz", vote_weighted = TRUE)
message("\nBUILDING SPOTIFY GENRE TREE")
build_genre_tree(s, "Spotify", vote_weighted = TRUE)

generate_report("02_genre_trees")


plot_network_graph(
  readRDS("models/trees/Spotify_graph.rds"),
  get_sizes_lookup(s),
  get_fills_lookup(s),
  height = 1000
)
