rm(list = ls())
gc()
devtools::load_all()

poptrag <- readRDS("data-raw/poptrag.rds")
s_genremapping <- readRDS("models/metagenres/tune_s_metagenres.rds")$solutions[[
  "1050"
]]$mapping

# # Prepare data sets for modeling ----
settings <- list(
  seed = 42,
  subsample_prop = 0.05,
  casewise_threshold = 0.4,
  artist_initial_split = 0.8,
  drop_POPULARMUSIC = TRUE,
  cv_folds = 10,
  cv_repeats = 1,
  max_tracks_per_artist_cv = 10000,
  s_genremapping = s_genremapping,
  min_n_factor_level = 100
)
data_low <- prepare_rf_data(
  settings,
  poptrag,
  read_feather_with_lists("models/metagenres/mb_metagenres_10_15.feather")
)
split_metagenre_dist_low <- compare_metagenre_distributions_splits(data_low)

data_high <- prepare_rf_data(
  settings,
  poptrag,
  read_feather_with_lists("models/metagenres/mb_metagenres_25_30.feather")
)
split_metagenre_dist_high <- compare_metagenre_distributions_splits(
  data_high
)

saveRDS(settings, "models/classifier/rf_data_settings.rds")
saveRDS(data_low, "models/classifier/data_low.rds")
saveRDS(data_high, "models/classifier/data_high.rds")
rm(list = ls())
gc()

message("Reading data...")
data_low <- readRDS("models/classifier/data_low.rds")
data_high <- readRDS("models/classifier/data_high.rds")

# # Impute missing values with missForestPredict ----
# data_imp_low <- train_and_impute_missing_data(
#   data_low,
#   n_cores = 8,
#   seed = 42,
#   maxiter_imp = 1,
#   max_rows_for_imputer = 50000,
#   max_missing_prop_for_imputer = 0.3
# )
# saveRDS(
#   data_imp_low$imputer_model,
#   "models/classifier/rf_imputer_model_low.rds"
# )
# saveRDS(data_imp_low$datasets, "models/classifier/data_low_imp.rds")

# data_imp_high <- train_and_impute_missing_data(
#   data_high,
#   n_cores = 4,
#   seed = 42,
#   maxiter_imp = 1,
#   max_rows_for_imputer = 50000,
#   max_missing_prop_for_imputer = 0.3
# )
# saveRDS(
#   data_imp_high$imputer_model,
#   "models/classifier/rf_imputer_model_high.rds"
# )
# saveRDS(data_imp_high$datasets, "models/classifier/data_high_imp.rds")

# Get model features ----
model_features <- colnames(data_low$train)
model_features <- model_features[
  !model_features %in%
    c(
      "track.s.id",
      "artist.s.id",
      "metagenre"
    )
]
# do not use Spotify genres as features
# model_features <- model_features[!grepl("^dtb\\.", model_features)]

# # Random Baseline Models ----
random_baseline_low <- train_random_baseline(
  data_low$train,
  data_low$test,
  list(seed = 42)
)
save_classification_model(
  random_baseline_low,
  "random_baseline_lowres"
)

random_baseline_high <- train_random_baseline(
  data_high$train,
  data_high$test,
  list(seed = 42)
)
save_classification_model(
  random_baseline_high,
  "random_baseline_highres"
)


# # GLMNET Models ----
settings <- list(
  seed = 42,
  glmnet_alpha = 0.5,
  model_features = model_features,
  n_cores = 1
)

glmnet_low <- train_glmnet(data_low, settings)
save_classification_model(
  glmnet_low,
  "glmnet_lowres",
  subfolder = "glmnet"
)

glmnet_high <- train_glmnet(data_high, settings)
save_classification_model(
  glmnet_high,
  "glmnet_highres",
  subfolder = "glmnet"
)
# show_important_predictors_glmnet(glmnet_low$model)
# show_important_predictors_glmnet(glmnet_high$model)

# Random forest models ----
settings <- list(
  seed = 42,
  ntrees = 3, # make 1000 for final
  n_cores = 19, # make large only for final
  varimp_top_n = 40,
  model_features = model_features,
  importance = "impurity", # better for tuning, final model is "permutation" by default
  tune_hyperparameters = TRUE,
  ntrees_tuning = 1, # make 100 for now
  initial_grid_density = 2, # adjust later
  bayes_iterations = 2, # adjust later
  uncertain_jump = 5,
  min.node.size_fix = 50,
  mtry_fix = 3,
  under_ratio_fix = 11
)
saveRDS(settings, "models/classifier/rf_tune_settings.rds")

message("---TRAINING LOW RESOLUTION MODEL---")
rf_low <- train_random_forest(
  data_low,
  settings
)
save_classification_model(
  rf_low,
  "rf_lowres",
  subfolder = "rf"
)

message("---TRAINING HIGH RESOLUTION MODEL---")
rf_high <- train_random_forest(
  data_high,
  settings
)
save_classification_model(
  rf_high,
  "rf_highres",
  subfolder = "rf"
)

# Generate resport ----
generate_report("05_rf_classifier")
