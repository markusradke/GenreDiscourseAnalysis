rm(list = ls())
devtools::load_all()

# Load data ----
mb <- read_feather_with_lists("data/filtered_mb_long.feather")
s <- read_feather_with_lists("data/filtered_s_long.feather")

# Build genre trees ----
message("\nBUILDING MUSICBRAINZ GENRE TREE")
build_genre_tree(mb, "MusicBrainz", vote_weighted = TRUE)
message("\nBUILDING SPOTIFY GENRE TREE")
build_genre_tree(s, "Spotify", vote_weighted = FALSE)

# Map initial genres ----
message("\n MAP INITIAL GENRES MUSICBRAINZ")
initial_genres_mb <- get_initial_genre_mapping(
  mb,
  readRDS("models/trees/MusicBrainz_graph.rds")
)
save_feather_with_lists(initial_genres_mb, "models/trees/initial_genres_mb")

message("\n MAP INITIAL GENRES SPOTIFY")
initial_genres_s <- get_initial_genre_mapping(
  s,
  readRDS("models/trees/Spotify_graph.rds")
)
save_feather_with_lists(initial_genres_s, "models/trees/initial_genres_s")

generate_report("02_genre_trees")
