rm(list = ls())
gc()
devtools::load_all()

mb <- read_feather_with_lists("data/filtered_mb_long.feather")
message("\nBUILDING MUSICBRAINZ GENRE TREE")
mb_res <- build_genre_tree(
  mb,
  "MusicBrainz",
  min_cooc = 30, # tag cooc must appear for at least 30 tracks to be considered (artist level?)
  vote_weighted = TRUE
)
generate_report("interactive_COMGET_browser_mb")
mb_support <- get_support_for_tree_edges(mb_res$final_tree, mb)
mb_artists <- dplyr::distinct(mb, trackartists.s.id, tag_name, .keep_all = TRUE)
mb_artist_support <- get_support_for_tree_edges(mb_res$final_tree, mb_artists)



message("\nBUILDING SPOTIFY GENRE TREE")
s <- read_feather_with_lists("data/filtered_s_long.feather")
s_res <- build_genre_tree(
  s,
  "Spotify",
  min_cooc = 1, # no filter
  vote_weighted = TRUE
)
generate_report("interactive_COMGET_browser_s")

# suport on artist level, i.e. how many artists are associated with both super and subgenre
s_artists <- dplyr::distinct(s, trackartists.s.id, tag_name, .keep_all = TRUE)
s_support <- get_support_for_tree_edges(s_res$final_tree, s_artists)
