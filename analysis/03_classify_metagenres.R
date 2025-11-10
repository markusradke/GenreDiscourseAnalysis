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
  metagenre_detail = "low"
)
rf_data_low <- prepare_rf_data(settings, poptrag)

settings$metagenre_detail <- "high"
rf_data_high <- prepare_rf_data(settings, poptrag)

save_feather_with_lists(
  rf_data_low$datasets$mb$train,
  "models/classifier/rf_mb_lowres_train.feather"
)
save_feather_with_lists(
  rf_data_low$datasets$mb$test,
  "models/classifier/rf_mb_lowres_test.feather"
)
save_feather_with_lists(
  rf_data_low$datasets$s$train,
  "models/classifier/rf_s_lowres_train.feather"
)
save_feather_with_lists(
  rf_data_low$datasets$s$test,
  "models/classifier/rf_s_lowres_test.feather"
)
save_feather_with_lists(
  rf_data_high$datasets$mb$train,
  "models/classifier/rf_mb_highres_train.feather"
)
save_feather_with_lists(
  rf_data_high$datasets$mb$test,
  "models/classifier/rf_mb_highres_test.feather"
)
save_feather_with_lists(
  rf_data_high$datasets$s$train,
  "models/classifier/rf_s_highres_train.feather"
)
save_feather_with_lists(
  rf_data_high$datasets$s$test,
  "models/classifier/rf_s_highres_test.feather"
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

settings <- list(
  seed = 42,
  undersample_factor = 10,
  n_cores = 19,
  varimp_top_n = 40,
  run_rf_mb = TRUE,
  run_rf_s = TRUE,
  features_after_impute = model_features
)

res_low <- train_and_evaluate_rf(
  settings,
  rf_data_low$datasets
)

res_high <- train_and_evaluate_rf(
  settings,
  rf_data_high$datasets
)

saveRDS(res_low$mb$evaluation, "models/classifier/rf_mb_lowres_eval.rds")
saveRDS(res_low$s$evaluation, "models/classifier/rf_s_lowres_eval.rds")
saveRDS(res_high$mb$evaluation, "models/classifier/rf_mb_highres_eval.rds")
saveRDS(res_high$s$evaluation, "models/classifier/rf_s_highres_eval.rds")

# Generate resport ----
generate_report("05_rf_classifier")
