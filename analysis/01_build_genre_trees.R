# Load data ----
mb <- read_feather_with_lists('data/filtered_mb_long.feather')
dc <- read_feather_with_lists('data/filtered_dc_long.feather')
s <- read_feather_with_lists('data/filtered_s_long.feather')

# Build genre trees ----
message("\nBUILDING MUSICBRAINZ GENRE TREE")
build_genre_tree(mb, 'MusicBrainz', vote_weighted = TRUE)
message("\nBUILDING DISCOGS GENRE TREE")
build_genre_tree(dc, 'Discogs', vote_weighted = FALSE)
message("\nBUILDING SPOTIFY GENRE TREE")
build_genre_tree(s, 'Spotify', vote_weighted = FALSE)

# Map initial genres ----
message("\n MAP INITIAL GENRES MUSICBRAINZ")
initial_genres_mb <- get_initial_genre_mapping(
  mb,
  readRDS('models/MusicBrainz_graph.rds')
)
save_feather_with_lists(initial_genres_mb, "models/initial_genres_s")

message("\n MAP INITIAL GENRES DISCOGS")
initial_genres_dc <- get_initial_genre_mapping(
  dc,
  readRDS('models/Discogs_graph.rds')
)
save_feather_with_lists(initial_genres_dc, "models/initial_genres_s")

message("\n MAP INITIAL GENRES SPOTIFY")
initial_genres_s <- get_initial_genre_mapping(
  s,
  readRDS('models/Spotify_graph.rds')
)
save_feather_with_lists(initial_genres_s, "models/initial_genres_s")

generate_report("02_genre_trees")
