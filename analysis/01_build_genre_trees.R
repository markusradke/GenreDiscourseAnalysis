message("Building genre tree for MusicBrainz...")
mb_tags <- readRDS("data/filtered_mb_long.rds")
mb_tree <- build_genre_tree(mb_tags, "musicbrainz", vote_weighted = TRUE)

message("Building genre tree for Discogs...")
dc_tags <- readRDS("data/filtered_dc_long.rds")
dc_tree <- build_genre_tree(dc_tags, "discogs", vote_weighted = FALSE)

message("Building genre tree for Spotify...")
s_tags <- readRDS("data/filtered_s_long.rds")
s_tree <- build_genre_tree(s_tags, "spotify", vote_weighted = FALSE)
