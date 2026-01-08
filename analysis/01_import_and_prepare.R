# Clean workspace and set parameters ----
rm(list = ls())
gc()
devtools::load_all()
# minimum number of unique artists a genre tag must be associated with to not be considered noise
settings <- list(
  n_artists_threshold = 50
)
saveRDS(settings, "data/settings_data_prep.rds")

message("Importing POPTRAG dataset and selecting variables ...")
poptrag <- readRDS("data-raw/poptrag.rds")
poptrag_selected <- poptrag |>
  dplyr::select(
    track.s.id,
    track.s.title,
    track.s.artists,
    track.s.firstartist.id,
    track.s.firstartist.name,
    track.s.previewurl,
    track.ab.genrerosamerica,
    track.dz.album.firstgenre.name,
    artist.s.id,
    artist.s.name,
    artist.s.genres,
    album.s.id,
    album.s.title,
    album.s.releaseyear,
    album.s.coverurl,
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
    n_trackartists = sapply(.data$track.s.artists, function(x) {
      as.integer(nrow(x))
    })
  )

saveRDS(poptrag_selected, "data/poptrag_selected.rds")

# filter specific problematic entries and drop obviously wrong Deezer album for one entry
poptrag_selected <- poptrag_selected |>
  dplyr::filter(.data$album.s.id != "3GmZxNPKzyfiD0urTJFbi3") |>
  dplyr::mutate(
    track.dz.album.firstgenre.name = ifelse(
      .data$album.s.id == "4zhRkgoZKC2xCPPys1gK4b",
      NA,
      .data$track.dz.album.firstgenre.name
    )
  )


# Prepare Musicbrainz ----
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

combined_mb_genres <- combine_mb_genres(poptrag_selected)
mb_long <- get_long_genre_tags(combined_mb_genres, "mb.genres")
mb_long_music <- filter_non_valid_tags(mb_long, mb_non_valid_tags)
mb_long_denoise_tags <- filter_tags_by_artist_occurrences(
  mb_long_music,
  n_min_artists = settings$n_artists_threshold
)
# additional filtering for Musicbrainz: min 2 votes per trag
mb_total_votes <- mb_long_denoise_tags |>
  dplyr::group_by(.data$track.s.id) |>
  dplyr::summarize(
    n_votes = sum(.data$tag_count),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$n_votes > 1) |>
  dplyr::select(track.s.id)
mb_long_denoise <- mb_long_denoise_tags |>
  dplyr::inner_join(mb_total_votes, by = "track.s.id")
save_feather_with_lists(mb_long_denoise, "data/filtered_mb_long.feather")

# Prepare Spotify ----
# Combine the genre tags of all artists involved in a track
# Infer votings based on the combination of artist genres
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
  "kabarett",
  "lo-fi sleep",
  "lo-fi study",
  "mediation",
  "sleep",
  "talent show",
  "talentschau",
  "kindermusik",
  "rain",
  "white noise"
)
saveRDS(s_non_music_tags, "data/s_non_music_tags.rds")

combined_s_genres <- combine_s_genres(poptrag_selected, spotify_artist_genres)
s_long <- get_long_genre_tags(combined_s_genres, "s.genres")
s_long_music <- filter_non_valid_tags(s_long, s_non_music_tags)
s_long_denoise <- filter_tags_by_artist_occurrences(
  s_long_music,
  n_min_artists = settings$n_artists_threshold
)
save_feather_with_lists(s_long_denoise, "data/filtered_s_long.feather")

# Generate data report ----
message("Generating data report ...")
generate_report("01_data_report")
