# Import PopTraG datset and select relevant variables ----
rm(list = ls())
gc()
n_artists_threshold <- 100

message("Importing PopTraG dataset and selecting variables ...")
poptrag <- readRDS("data-raw/poptrag.rds")
poptrag_selected <- poptrag |>
  dplyr::select(
    track.s.id,
    track.s.title,
    track.s.artists,
    track.s.firstartist.name,
    artist.s.id,
    artist.s.name,
    artist.s.genres,
    album.s.id,
    album.s.title,
    album.dc.id,
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
  ) |>
  dplyr::mutate(
    trackartists.s.id = sapply(.data$track.s.artists, function(x) {
      paste(x$id, collapse = ";")
    }),
    n_artists = sapply(.data$track.s.artists, function(x) as.integer(nrow(x)))
  )

saveRDS(poptrag_selected, "data/poptrag_selected.rds")

# Denoise Musicbrainz ----
message(
  "Filtering out tracks without valid MusicBrainz genre tags and tags with too few occurences  ..."
)
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
mb_whitelist <- readRDS("data-raw/musicbrainz_genre_whitelist_20250616.rds")
unique_mb_tags <- get_unique_mb_tags(poptrag_selected)
non_whitelist_genres <- setdiff(unique_mb_tags, mb_whitelist)
mb_non_valid_tags <- union(mb_non_music_tags, non_whitelist_genres)

filtered_mb_genre <- filter_valid_mb_genres(
  poptrag_selected,
  mb_non_valid_tags
)
mb_long <- get_long_genre_tags(filtered_mb_genre, "mb.genres")
save_feather_with_lists(mb_long, "data/filtered_mb_long")

# Denoise Spotify ----
# Combine the genre tags of all artists involved in a track
spotify_artist_genres <- readRDS("data-raw/spotify_artist_genres_lookup.rds")
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
  "talentschau",
  "kindermusik"
)
saveRDS(s_non_music_tags, "data/s_non_music_tags.rds")
message(
  "Filtering out tracks without valid Spotify genre tags and tags with too few occurences  ..."
)
filtered_s_genres <- filter_valid_s_genres(
  poptrag_selected,
  s_non_music_tags,
  spotify_artist_genres
)
s_long <- get_long_genre_tags(
  filtered_s_genres,
  "s.genres"
)
save_feather_with_lists(s_long, "data/filtered_s_long")

# Generate data report ----
message("Generating data report ...")
generate_report("01_data_report")
