rm(list = ls())
gc()
devtools::load_all()

poptrag <- readRDS("data-raw/poptrag.rds")
s_genremapping <- readRDS("models/metagenres/tune_s_metagenres.rds")$solutions[[
  "1375"
]]$mapping

# Prepare data sets for modeling ----
settings <- list(
  seed = 42,
  subsample_prop = 1, # later 1
  casewise_threshold = 0.4,
  artist_initial_split = 0.8, # later 0.8
  apply_imputation = FALSE,
  n_cores = 32,
  drop_POPULARMUSIC = TRUE,
  cv_folds = 3, # later 5?
  cv_repeats = 1, # later 2?
  max_tracks_per_artist_cv = 10000,
  s_genremapping = s_genremapping,
  maxiter_imp = 1
)
rf_data <- prepare_rf_data(settings, poptrag)

saveRDS(rf_data$low, "models/classifier/rf_data_low.rds")
saveRDS(rf_data$high, "models/classifier/rf_data_high.rds")

# Train models ----
rf_data <- list(
  low = readRDS("models/classifier/rf_data_low.rds"),
  high = readRDS("models/classifier/rf_data_high.rds")
)

model_features <- rf_prep_features[
  !rf_prep_features %in% c("artist.s.genres", "track.s.id", "artist.s.id")
]
distribution_genres <- rbind(rf_data$low$train, rf_data$low$test) |>
  dplyr::select(dplyr::contains("dtb.")) |>
  colnames()
model_features <- c(model_features, distribution_genres)


# debugging
# model_features <- c(
#   "track.s.speechiness",
#   "artist.s.followers"
# )

# Baseline models ----
# settings <- list(
#   seed = 42,
#   n_cores = 19,
#   run_random_low = TRUE,
#   run_random_high = TRUE,
#   run_glmnet_low = FALSE, # Takes too long with that many features
#   run_glmnet_high = FALSE,
#   model_features = model_features,
#   glmnet_alpha = 0.5 # (0=ridge, 1=lasso, default=0.5)
# )
# baseline_res <- train_and_evaluate_baselines(settings, rf_data)

# Random forest models ----
settings <- list(
  seed = 42,
  ntrees = 1000, # make 1000 for final
  ntrees_tuning = 100, # make 100 for now
  under_ratio = 11, # irrelevant for tuning
  n_cores = 32, # make large only for final
  mtry = 72, # irrelevant for tuning
  min.node.size = 50, # irrelevant for tuning
  varimp_top_n = 40,
  run_rf_low = TRUE,
  run_rf_high = FALSE, # make seperate to spare memory
  features_after_impute = model_features,
  tune_hyperparameters = TRUE,
  importance = "impurity", # better for tuning, final model is "permutation" by default
  uncertain_jump = 5,
  bayes_iterations = 20, # adjust later
  initial_grid_density = 3 # adjust later
)

message("---TRAINING LOW RESOLUTION MODEL---")
start_time <- Sys.time()
res_rf_low <- train_and_evaluate_rf_tidy(
  settings,
  rf_data
)
end_time <- Sys.time()
time_needed <- end_time - start_time
message(paste("Time needed for training low resolution model:", time_needed))
res_rf_low$low$evaluation$time_needed <- time_needed

message("Saving model evaluation and settings...")
saveRDS(res_rf_low$low$evaluation, "models/classifier/rf_mb_lowres_eval.rds")
saveRDS(
  res_rf_low$low$model_settings,
  "models/classifier/rf_mb_lowres_settings.rds"
)
message("Saving tuning plots...")
plot_tuning_results(res_rf_low$low$tuning_results)
ggplot2::ggsave(
  "models/classifier/rf_mb_lowres_tuning_parameters.png",
  width = 12,
  height = 8
)
plot_tuning_performance(res_rf_low$low$tuning_results, metric = "f_meas")
ggplot2::ggsave(
  "models/classifier/rf_mb_lowres_tuning_performance.png",
  width = 8,
  height = 6
)
message("Saving best model...")
saveRDS(res_rf_low$low$model, "models/classifier/best_model_low.rds")

message("---TRAINING HIGH RESOLUTION MODEL---")
settings$run_rf_low <- FALSE
settings$run_rf_high <- TRUE
start_time <- Sys.time()
res_rf_high <- train_and_evaluate_rf_tidy(
  settings,
  rf_data
)
end_time <- Sys.time()
time_needed <- end_time - start_time
message(paste("Time needed for training high resolution model:", time_needed))
res_rf_high$high$evaluation$time_needed <- time_needed

message("Saving model evaluation and settings...")
saveRDS(res_rf_high$high$evaluation, "models/classifier/rf_mb_highres_eval.rds")
saveRDS(
  res_rf_high$high$model_settings,
  "models/classifier/rf_mb_highres_settings.rds"
)
message("Saving tuning plots...")
plot_tuning_results(res_rf_high$high$tuning_results)
ggplot2::ggsave(
  "models/classifier/rf_mb_highres_tuning_parameters.png",
  width = 12,
  height = 8
)
plot_tuning_performance(res_rf_high$high$tuning_results, metric = "f_meas")
ggplot2::ggsave(
  "models/classifier/rf_mb_highres_tuning_performance.png",
  width = 8,
  height = 6
)
message("Saving best model...")
saveRDS(res_rf_high$high$model, "models/classifier/best_model_high.rds")


# Generate resport ----
generate_report("05_rf_classifier")
