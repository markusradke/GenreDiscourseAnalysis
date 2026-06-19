# Clean workspace and set parameters ----
rm(list = ls())
gc()
library(dplyr)
library(readr)
devtools::load_all()


message("Importing POPTRAG dataset and selecting variables ...")
poptrag <- readRDS("data-raw/poptrag.rds")
poptrag_selected <- select_relevant_columns(poptrag)
saveRDS(poptrag_selected, "data/poptrag_selected.rds")
poptrag_selected <- readRDS("data/poptrag_selected.rds")

# filter specific problematic entries and drop obviously wrong Deezer album for one entry
poptrag_selected <- poptrag_selected |>
  filter(.data$album.s.id != "3GmZxNPKzyfiD0urTJFbi3") |>
  mutate(
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
  "standup comedy",
  "audio drama"
)
saveRDS(mb_non_music_tags, "data/mb_non_music_tags.rds")
mb_whitelist <- readRDS("data-raw/musicbrainz_genre_whitelist_20250616.rds")
unique_mb_tags <- get_unique_mb_tags(poptrag_selected)
non_whitelist_genres <- setdiff(unique_mb_tags, mb_whitelist)
mb_non_valid_tags <- union(mb_non_music_tags, non_whitelist_genres)

combined_mb_genres <- combine_mb_genres(poptrag_selected)
mb_long <- get_long_genre_tags(combined_mb_genres, "mb.genres")
mb_long_music <- filter_non_valid_tags(mb_long, mb_non_valid_tags)

mb_long_denoise <- filter_min_votes(
  mb_long_music,
  n_min_votes = 2
)

mb_final <- mb_long_denoise |>
  select(-dplyr::matches("genres$"))

write_csv(mb_final, "data/filtered_mb_long.csv")

# Prepare Spotify ----
# TODO: FINISH
# Combine the genre tags of all artists involved in a track
# Infer votings based on the combination of artist genres
# spotify_artist_genres <- readRDS("data-raw/spotify_artist_genres_lookup.rds")
# s_non_music_tags <- c(
#   "432hz",
#   "528hz",
#   "asmr",
#   "binaural",
#   "children's folk",
#   "children's music",
#   "clean comedy",
#   "comic",
#   "escape room",
#   "field recording ambient",
#   "football",
#   "hoerspiel",
#   "kabarett",
#   "lo-fi sleep",
#   "lo-fi study",
#   "mediation",
#   "sleep",
#   "talent show",
#   "talentschau",
#   "kindermusik",
#   "rain",
#   "white noise"
# )
# saveRDS(s_non_music_tags, "data/s_non_music_tags.rds")

# combined_s_genres <- combine_s_genres(poptrag_selected, spotify_artist_genres)
# s_long <- get_long_genre_tags(combined_s_genres, "s.genres")
# s_long_music <- filter_non_valid_tags(s_long, s_non_music_tags)
# s_final <- mb_long_denoise |>
#   dplyr::select(-dplyr::matches("genres$"))
# readr::write_csv(s_final, "data/filtered_s_long.csv")
