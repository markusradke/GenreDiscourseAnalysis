# Import PopTraG datset and select relevant variables ----
rm(list = ls())
gc()

message("Importing PopTraG dataset and selecting variables ...")
poptrag <- readRDS("data-raw/poptrag.rds")
poptrag_selected <- poptrag |>
  dplyr::select(
    track.s.id,
    track.s.title,
    track.s.firstartist.name,
    artist.s.id,
    artist.s.name,
    artist.s.genres,
    album.s.id,
    album.s.title,
    album.dc.genres,
    album.dc.styles,
    album.dc.firstgenre,
    track.mb.genres,
    album.mb.genres,
    artist.mb.genres,
    track.mb.combinedgenre,
    source.officialcharts,
    source.recommendations,
    source.featuredplaylists,
    source.spotifycharts
  )
saveRDS(poptrag_selected, "data/poptrag_selected.rds")

# Filter out tracks that are in only in Spotify Charts ----
message("Filtering out tracks that are only in Spotify Charts ...")
poptrag_wo_spotifycharts <- poptrag_selected |>
  dplyr::filter(
    source.officialcharts |
      source.recommendations |
      source.featuredplaylists
  )
saveRDS(
  poptrag_wo_spotifycharts,
  "data/poptrag_selected_wout_spotify_charts.rds"
)


# Filter out tracks with valid MusicBrainz genre tags ----
# TODO? filter out non-whitelist tags
message("Filtering out tracks without valid MusicBrainz genre tags ...")
mb_non_music_tags <- c(
  "non-music",
  "interview",
  "comedy",
  "nature sounds",
  "children's music",
  "spoken word",
  "standup comedy"
)
saveRDS(mb_non_music_tags, "data/mb_non_music_tags.rds")

filtered_mb_genre <- filter_valid_mb_genres(
  poptrag_wo_spotifycharts,
  mb_non_music_tags
)
mb_long <- get_long_genre_tags(filtered_mb_genre, "mb.genres")
save_feather_with_lists(mb_long, "data/filtered_mb_long")

# Filter out tracks without any entries for Discogs genres and styles ----
dc_non_music_tags <- c(
  "Non-Music",
  "Children's",
  "Audiobook",
  "Comedy",
  "Speech",
  "Spoken Word"
)
saveRDS(dc_non_music_tags, "data/dc_non_music_tags.rds")
message("Filtering out tracks without valid Discogs genre tags ...")
filtered_dc_genres <- filter_valid_dc_genres(
  poptrag_wo_spotifycharts,
  dc_non_music_tags
)
dc_long <- get_long_genre_tags(filtered_dc_genres, "dc.genres")
save_feather_with_lists(dc_long, "data/filtered_dc_long")

# Filter out tracks without any entries for Spotify genres ----
s_non_music_tags <- c(
  "432hz",
  "528hz",
  "asmr",
  "binaural",
  "children's folk",
  "children's music",
  "clean comedy",
  "comic",
  "escape room",
  "field recording ambient",
  "football",
  "lo-fi sleep",
  "lo-fi study",
  "mediation",
  "sleep",
  "talent show",
  "talentschau"
)
saveRDS(s_non_music_tags, "data/s_non_music_tags.rds")
message("Filtering out tracks without valid Spotify genre tags ...")
filtered_s_genres <- filter_valid_s_genres(
  poptrag_wo_spotifycharts,
  s_non_music_tags
)
s_long <- get_long_genre_tags(filtered_s_genres, "s.genres")
save_feather_with_lists(s_long, "data/filtered_s_long")
