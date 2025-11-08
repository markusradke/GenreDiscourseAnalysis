# PROTOTYPE MODEL ----
poptrag <- readRDS("data-raw/poptrag.rds")
model_features <- c(
  "metagenre", # including outcome
  "track.s.danceability",
  "track.s.energy",
  "track.s.acousticness",
  "track.s.instrumentalness",
  "track.s.valence",
  "track.s.tempo",
  "track.s.loudness",
  "track.s.speechiness",
  "track.s.liveness",
  "track.s.popularity",
  "track.s.duration",
  "track.language",
  "artist.s.followers",
  "artist.mb.birthyear",
  "artist.mb.gender",
  "artist.mb.type",
  "artist.mb.area"
)

settings <- list(
  subsample_prop = 0.2,
  casewise_threshold = 0.4,
  seed = 42,
  artist_initial_split = 0.5,
  apply_imputation = FALSE,
  undersample_factor = 10,
  n_cores = 19,
  varimp_top_n = 40,
  drop_POPULARMUSIC = TRUE,
  metagenre_detail = "low",
  run_rf_mb = TRUE,
  run_rf_s = TRUE,
  features_after_impute = model_features
)
res <- run_rf_pipeline(settings, poptrag)
export_mb <- list(
  confusion = res$mb$evaluation$confusion,
  metrics = res$mb$evaluation$metrics,
  varimp = res$mb$evaluation$varimp
)
saveRDS(export_mb, "models/classifier/rf_mb_lowres_eval.rds")
export_s <- list(
  confusion = res$s$evaluation$confusion,
  metrics = res$s$evaluation$metrics,
  varimp = res$s$evaluation$varimp
)
saveRDS(export_s, "models/classifier/rf_s_lowres_eval.rds")

settings <- list(
  subsample_prop = 0.2,
  casewise_threshold = 0.4,
  seed = 42,
  artist_initial_split = 0.5,
  apply_imputation = FALSE,
  undersample_factor = 10,
  n_cores = 19,
  varimp_top_n = 40,
  drop_POPULARMUSIC = TRUE,
  metagenre_detail = "high", # "low" or "high"
  run_rf_mb = TRUE,
  run_rf_s = TRUE,
  features_after_impute = model_features
)
res <- run_rf_pipeline(settings, poptrag)
export_mb <- list(
  confusion = res$mb$evaluation$confusion,
  metrics = res$mb$evaluation$metrics,
  varimp = res$mb$evaluation$varimp
)
saveRDS(export_mb, "models/classifier/rf_mb_highres_eval.rds")
export_s <- list(
  confusion = res$s$evaluation$confusion,
  metrics = res$s$evaluation$metrics,
  varimp = res$s$evaluation$varimp
)
saveRDS(export_s, "models/classifier/rf_s_highres_eval.rds")

generate_report("04_rf_classifier")
