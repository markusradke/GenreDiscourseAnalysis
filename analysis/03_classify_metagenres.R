devtools::load_all()
# PROTOTYPE MODEL ----
poptrag <- readRDS("data-raw/poptrag.rds")

# Prepare data sets for modeling ----
settings <- list(
  seed = 42,
  subsample_prop = 1,
  casewise_threshold = 0.4,
  artist_initial_split = 0.8,
  apply_imputation = FALSE,
  n_cores = 19,
  drop_POPULARMUSIC = TRUE,
  cv_folds = 5,
  cv_repeats = 2,
  max_tracks_per_artist_cv = 10000
)
rf_data <- prepare_rf_data(settings, poptrag)
dist_low <- compare_metagenre_distributions(rf_data, detail_level = "low")
dist_low$plot
dist_high <- compare_metagenre_distributions(rf_data, detail_level = "high")
dist_high$plot

save_feather_with_lists(
  rf_data$low$train,
  "models/classifier/rf_mb_lowres_train.feather"
)
save_feather_with_lists(
  rf_data$low$test,
  "models/classifier/rf_mb_lowres_test.feather"
)
save_feather_with_lists(
  rf_data$high$train,
  "models/classifier/rf_mb_highres_train.feather"
)
save_feather_with_lists(
  rf_data$high$test,
  "models/classifier/rf_mb_highres_test.feather"
)


# Run random forest training ----

model_features <- c(
  "artist.s.popularity",
  "artist.s.followers",
  "album.s.totaltracks",
  "album.s.releaseyear",
  "album.s.popularity",
  "track.s.danceability",
  "track.s.energy",
  "track.s.key",
  "track.s.loudness",
  "track.s.mode",
  "track.s.speechiness",
  "track.s.acousticness",
  "track.s.instrumentalness",
  "track.s.liveness",
  "track.s.valence",
  "track.s.tempo",
  "track.s.timesignature",
  "track.s.explicitlyrics",
  "track.s.popularity",
  "track.s.duration",
  "artist.mb.gender",
  "artist.mb.area",
  "artist.mb.birthyear",
  "artist.mb.dead",
  "track.ab.p.danceable",
  "track.ab.p.female",
  "track.ab.p.acoustic",
  "track.ab.p.aggressive",
  "track.ab.p.electronic",
  "track.ab.p.happy",
  "track.ab.p.party",
  "track.ab.p.relaxed",
  "track.ab.p.sad",
  "track.ab.p.bright",
  "track.ab.p.tonal",
  "track.ab.p.voice",
  "track.ab.rhythm.tempo",
  "track.ab.rhythm.danceability",
  "track.ab.rhythm.onsetrate",
  "track.ab.low.loudness",
  "track.ab.low.dynamiccomplexity",
  "track.ab.tonal.chordchangerate",
  "track.ab.tonal.key",
  "track.ab.tonal.chordsnumberrate",
  "track.ab.tonal.mode",
  "track.ab.tonal.keystrength",
  "track.dz.rank",
  "track.dz.tempo",
  "track.dz.loudness",
  "track.dz.firstartist.followers",
  "track.dz.firstartist.nalbums",
  "track.dz.album.explicitlyrics",
  "track.dz.album.duration",
  "track.dz.album.followers",
  "track.language",
  "track.is.instrumental",
  "lyrics.distinct_words_ratio",
  "lyrics.repeated_lines_ratio",
  "lyrics.sentiment",
  "lyrics.nrc_anger",
  "lyrics.nrc_anticipation",
  "lyrics.nrc_disgust",
  "lyrics.nrc_fear",
  "lyrics.nrc_joy",
  "lyrics.nrc_sadness",
  "lyrics.nrc_surprise",
  "lyrics.nrc_trust",
  "lyrics.nrc_negative",
  "lyrics.nrc_positive",
  "track.is.dach",
  "label.med.artist.popularity",
  "label.med.album.popularity",
  "label.med.track.popularity"
)

sqrt(length(model_features))
settings <- list(
  seed = 42,
  undersample_factor = 2,
  n_cores = 19,
  ntrees = 1000,
  mtry = 5,
  min.node.size = 50,
  max.depth = 20,
  varimp_top_n = 40,
  run_rf_low = TRUE,
  run_rf_high = TRUE,
  features_after_impute = model_features
)

res_rf <- train_and_evaluate_rf(
  settings,
  rf_data
)

saveRDS(res_rf$low$evaluation, "models/classifier/rf_mb_lowres_eval.rds")
saveRDS(
  res_rf$low$model_settings,
  "models/classifier/rf_mb_lowres_settings.rds"
)
saveRDS(res_rf$high$evaluation, "models/classifier/rf_mb_highres_eval.rds")
saveRDS(
  res_rf$high$model_settings,
  "models/classifier/rf_mb_highres_settings.rds"
)
# Generate resport ----
generate_report("05_rf_classifier")
