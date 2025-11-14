rm(list = ls())
gc()
devtools::load_all()

poptrag <- readRDS("data-raw/poptrag.rds")
s_genremapping <- readRDS("models/metagenres/tune_s_metagenres.rds")$solutions[[
  "1050"
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
  ntrees = 100, # make 1000 for final
  ntrees_tuning = 10, # make 100 for now
  under_ratio = 8, # irrelevant for tuning
  n_cores = 1,
  mtry = 15, # irrelevant for tuning
  min.node.size = 50, # irrelevant for tuning
  varimp_top_n = 40,
  run_rf_low = TRUE,
  run_rf_high = FALSE, # make seperate to spare memory
  features_after_impute = model_features,
  tune_hyperparameters = TRUE,
  importance = "impurity", # better for tuning, final model is "permutation" by default
  uncertain_jump = 5,
  bayes_iterations = 10, # adjust later
  initial_grid_density = 2 # adjust later
)

res_rf_low <- train_and_evaluate_rf_tidy(
  settings,
  rf_data
)
saveRDS(res_rf_low$low$evaluation, "models/classifier/rf_mb_lowres_eval.rds")
saveRDS(
  res_rf_low$low$model_settings,
  "models/classifier/rf_mb_lowres_settings.rds"
)
saveRDS(res_rf_low$low, "models/classifier/results_low.rds")

settings$run_rf_low <- FALSE
settings$run_rf_high <- TRUE
res_rf_high <- train_and_evaluate_rf_tidy(
  settings,
  rf_data
)
saveRDS(res_rf_high$high$evaluation, "models/classifier/rf_mb_highres_eval.rds")
saveRDS(
  res_rf_high$high$model_settings,
  "models/classifier/rf_mb_highres_settings.rds"
)
saveRDS(res_rf_high$high, "models/classifier/results_high.rds")


# inspect plots
plot_cm(
  res_rf$low$evaluation$confusion_train,
  res_rf$low$evaluation$metrics_train
)
plot_cm(
  res_rf$low$evaluation$confusion_test,
  res_rf$low$evaluation$metrics_test
)
plot_tuning_results(res_rf$low$tuning_results)
plot_tuning_performance(res_rf$low$tuning_results)
plot_varimp(
  res_rf$low$evaluation$varimp,
  top_n = 40
)
# Generate resport ----
generate_report("05_rf_classifier")
