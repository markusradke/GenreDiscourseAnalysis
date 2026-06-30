# Clean workspace and set parameters ----
rm(list = ls())
gc()
library(dplyr)
library(readr)
devtools::load_all()


message("Importing POPTRAG dataset and selecting variables ...")
poptrag <- readRDS("data-raw/poptrag.rds")
poptrag_selected <- select_relevant_columns(poptrag)

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

saveRDS(poptrag_selected, "data/poptrag_selected.rds")
poptrag_selected <- readRDS("data/poptrag_selected.rds")

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
saveRDS(mb_non_music_tags, "data/long_data/mb_non_music_tags.rds")
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

write_csv(mb_final, "data/long_data/mb_long.csv")

# Prepare Rosamerica ----
ab_long <- poptrag_selected |>
  filter(
    !is.na(track.ab.genrerosamerica) & track.ab.genrerosamerica != "speech" # discard speech tracks
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::contains("ab.p.rosa."),
    names_to = "tag_name",
    values_to = "tag_count"
  ) |>
  select(track.s.id, track.s.firstartist.id, tag_name, tag_count) |>
  mutate(tag_name = stringr::str_remove(tag_name, "track.ab.p.rosa.")) |>
  filter(tag_name != "speech") # discard probability for speech
ab_long |>
  group_by(tag_name) |>
  summarise(n = sum(tag_count)) |>
  arrange(desc(n))
readr::write_csv(ab_long, "data/long_data/ab_long.csv")

# Prepare Discogs ----
dc <- poptrag_selected |>
  filter(!is.na(album.dc.genres_str))
dc_long <- dc |>
  tidyr::separate_rows(album.dc.genres_str, sep = "; ") |>
  rename(tag_name = album.dc.genres_str) |>
  select(track.s.id, track.s.firstartist.id, tag_name) |>
  mutate(tag_count = 1L) |>
  filter(!tag_name %in% c("Non-Music", "Children's", ""))
count(dc_long, tag_name, sort = TRUE)
readr::write_csv(dc_long, "data/long_data/dc_long.csv")

# Prepare Deezer ----
dz <- poptrag_selected |>
  filter(!is.na(album.dz.genres_str))
dz_long <- dz |>
  tidyr::separate_rows(album.dz.genres_str, sep = "; ") |>
  rename(tag_name = album.dz.genres_str) |>
  select(track.s.id, track.s.firstartist.id, tag_name) |>
  mutate(tag_count = 1L) |>
  filter(
    !tag_name %in%
      c("Comedy", "Hörbücher", "Kids", "Hörbücher auf Deutsch", "Schlaflieder")
  )
count(dz_long, tag_name, sort = TRUE) |> print(n = 100)
readr::write_csv(dz_long, "data/long_data/dz_long.csv")

# Prepare Spotify ----
poptrag_selected$artist.s.genre_str <- sapply(
  poptrag_selected$artist.s.genres,
  function(x) paste(x$genre, collapse = "; ")
)
poptrag_selected$artist.s.genre_str[
  poptrag_selected$artist.s.genre_str == ""
] <- NA
s_long <- select(
  poptrag_selected,
  track.s.id,
  track.s.firstartist.id,
  artist.s.genre_str
) |>
  tidyr::separate_rows(artist.s.genre_str, sep = "; ") |>
  rename(tag_name = artist.s.genre_str) |>
  mutate(tag_count = 1L) |>
  filter(!is.na(tag_name))

s_non_music_tags <- c(
  "talent show"
)
saveRDS(s_non_music_tags, "data/s_non_music_tags.rds")
s_long <- filter(s_long, !tag_name %in% s_non_music_tags)


readr::write_csv(s_long, "data/long_data/s_long.csv")
