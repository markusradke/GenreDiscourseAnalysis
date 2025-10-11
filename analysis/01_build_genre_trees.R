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
# TODO: FIX MAPPING
# TODO: OPTIMIZE (takes very long for large datasets)
message("\n MAP INITIAL GENRES MUSICBRAINZ")
initial_genres_mb <- get_initial_genre_mapping(
  mb_test,
  readRDS('models/MusicBrainz_graph.rds')
)

message("\n MAP INITIAL GENRES DISCOGS")
initial_genres_dc <- get_initial_genre_mapping(
  dc_test,
  readRDS('models/Discogs_graph.rds')
)

message("\n MAP INITIAL GENRES SPOTIFY")
initial_genres_s <- get_initial_genre_mapping(
  s_test,
  readRDS('models/Spotify_graph.rds')
)

# RANDOM QUICK FIX FOR MAPPING (initial genres_needed for visualization of trees) ----
initial_genres_mb <- dplyr::distinct(mb, track.s.id, .keep_all = TRUE) |>
  dplyr::select(-tag_name, -tag_count)
genres_graph_mb <- igraph::V(readRDS('models/MusicBrainz_graph.rds'))$name
initial_genres_mb$initial_genres <- rep(
  genres_graph_mb,
  length.out = nrow(initial_genres_mb)
)
save_feather_with_lists(
  initial_genres_mb,
  'data/initial_genres_mb.feather'
)

initial_genres_dc <- dplyr::distinct(dc, track.s.id, .keep_all = TRUE) |>
  dplyr::select(-tag_name, -tag_count)
genres_graph_dc <- igraph::V(readRDS('models/Discogs_graph.rds'))$name
initial_genres_dc$initial_genres <- rep(
  genres_graph_dc,
  length.out = nrow(initial_genres_dc)
)
save_feather_with_lists(
  initial_genres_dc,
  'data/initial_genres_dc.feather'
)

initial_genres_s <- dplyr::distinct(s, track.s.id, .keep_all = TRUE) |>
  dplyr::select(-tag_name, -tag_count)
genres_graph_s <- igraph::V(readRDS('models/Discogs_graph.rds'))$name
initial_genres_s$initial_genres <- rep(
  genres_graph_s,
  length.out = nrow(initial_genres_s)
)
save_feather_with_lists(
  initial_genres_s,
  'data/initial_genres_s.feather'
)
