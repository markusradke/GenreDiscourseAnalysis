# PROTOTYPE MODEL ----
poptrag <- readRDS("data-raw/poptrag.rds")

# Prepare data sets for modeling ----
settings <- list(
  seed = 42,
  subsample_prop = 0.2,
  casewise_threshold = 0.4,
  artist_initial_split = 0.5,
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
generate_report("04_rf_classifier")
