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

# Analyze edges with low artist counts ----
# How many artist coocurencres do we need to trust an edge?
show_edges_with_low_artist_count <- function(graph, data, threshold = 50) {
  edges <- igraph::as_data_frame(graph, what = "edges")
  edge_counts <- edges |>
    dplyr::rowwise() |>
    dplyr::mutate(
      count = nrow(
        data |>
          dplyr::filter(
            tag_name %in% c(from, to)
          ) |>
          dplyr::group_by(track.s.firstartist.id) |>
          dplyr::filter(dplyr::n() == 2)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(count < threshold)
  edge_counts
}

# takes a small while to run
show_edges_with_low_artist_count(
  readRDS("models/trees/Spotify_graph.rds"),
  s,
  threshold = 50
)

show_edges_with_low_artist_count(
  readRDS("models/trees/MusicBrainz_graph.rds"),
  mb,
  threshold = 50
)
