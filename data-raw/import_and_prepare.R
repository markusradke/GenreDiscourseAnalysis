# Import PopTraG datset and select relevant variables ----
poptrag <- readRDS('data-raw/poptrag.rds')
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
saveRDS(poptrag_selected, 'data/poptrag_selected.rds')

# Filter out tracks that are in only in Spotify Charts ----
poptrag_selected_wout_spotify_charts <- poptrag_selected |>
  dplyr::filter(
    source.officialcharts |
      source.recommendations |
      source.featuredplaylists
  )
saveRDS(
  poptrag_selected_wout_spotify_charts,
  'data/poptrag_selected_wout_spotify_charts.rds'
)


# Filter out tracks without any entries for MusicBrainz genre tags and non-music tags ----
# TODO? filter out non-whitelist tags
mb_non_music_tags <- c(
  'non-music',
  'interview',
  'comedy',
  'nature sounds',
  "children's music",
  'spoken word',
  'standup comedy'
)
saveRDS(mb_non_music_tags, 'data/mb_non_music_tags.rds')

poptrag_filtered_mb_genre <- filter_valid_mb_genres(
  poptrag_selected_wout_spotify_charts,
  mb_non_music_tags
)
saveRDS(poptrag_filtered_mb_genre, 'data/poptrag_filtered_mb_genre.rds')

mb_tags <- unpack_mb_genre_tags(poptrag_filtered_mb_genre)
saveRDS(mb_tags, 'data/mb_tags.rds')

# TODO: go on here with Discogs tags
# Filter out tracks without any entries for Discogs genres and styles ----
filtered_valid_dc_genres <- poptrag_selected_wout_spotify_charts %>%
  dplyr::filter(!is.na(album.dc.firstgenre)) %>%
  dplyr::filter(
    album.dc.firstgenre != 'Non-Music' & album.dc.firstgenre != "Children's"
  )
saveRDS(filtered_valid_dc_genres, 'data/filtered_valid_dc_genres.rds')

# Unpack Discogs genre and style tags ----
