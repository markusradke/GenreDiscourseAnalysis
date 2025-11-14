rm(list = ls())
gc()
devtools::load_all()

mb <- read_feather_with_lists("data/filtered_mb_long.feather")
s <- read_feather_with_lists("data/filtered_s_long.feather")

# Build genre trees ----
quantile_threhold_for_edges <- 0.0
message("\nBUILDING MUSICBRAINZ GENRE TREE")
build_genre_tree(
  mb,
  "MusicBrainz",
  vote_weighted = TRUE,
  quantile_threhold_for_edges = quantile_threhold_for_edges
)
message("\nBUILDING SPOTIFY GENRE TREE")
build_genre_tree(
  s,
  "Spotify",
  vote_weighted = TRUE,
  quantile_threhold_for_edges = quantile_threhold_for_edges
)

generate_report("02_genre_trees")
message("Done.")
