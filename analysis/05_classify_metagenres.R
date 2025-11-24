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
  subsample_prop = 1,
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

data_high <- prepare_rf_data(
  settings,
  poptrag,
  read_feather_with_lists("models/metagenres/mb_metagenres_25_30.feather")
)

saveRDS(settings, "models/classifier/data_settings.rds")
saveRDS(data_low, "models/classifier/data_low.rds")
saveRDS(data_high, "models/classifier/data_high.rds")
rm(list = ls())
gc()

# Impute missing values with missForestPredict ----
settings <- list(
  n_cores = 8,
  seed = 42,
  maxiter_imp = 1,
  max_rows_for_imputer = 15000,
  max_missing_prop_for_imputer = 0.3
)

message("Reading data...")
data_low <- readRDS("models/classifier/data_low.rds")

imputer_low <- train_imputer(
  data_low$train,
  nthreads = settings$n_cores,
  seed = settings$seed,
  maxiter = settings$maxiter_imp,
  max_rows = settings$max_rows_for_imputer,
  max_missing_prop = settings$max_missing_prop_for_imputer
)
saveRDS(
  data_imp_low$imputer_model,
  "models/classifier/imputer/rf_imputer_model_low.rds"
)

train_imputed <- impute_data(data_low$train, imputer_low)
saveRDS(train_imputed, "models/classifier/imputer/train_low_imputed.rds")
test_imputed <- impute_data(data_low$test, imputer_low)
saveRDS(test_imputed, "models/classifier/imputer/test_low_imputed.rds")
cv_splits_imputed <- impute_cv_splits(data_low$cv_splits, imputer_low)
saveRDS(
  cv_splits_imputed,
  "models/classifier/imputer/cv_splits_low_imputed.rds"
)

rm(list = ls()[ls() != "settings"])
gc()
data_high <- readRDS("models/classifier/data_high.rds")
imputer_high <- train_imputer(
  data_high$train,
  nthreads = settings$n_cores,
  seed = settings$seed,
  maxiter = settings$maxiter_imp,
  max_rows = settings$max_rows_for_imputer,
  max_missing_prop = settings$max_missing_prop_for_imputer
)
saveRDS(
  data_imp_high$imputer_model,
  "models/classifier/imputer/rf_imputer_model_high.rds"
)
train_imputed <- impute_data(data_high$train, imputer_high)
saveRDS(train_imputed, "models/classifier/imputer/train_high_imputed.rds")
test_imputed <- impute_data(data_high$test, imputer_high)
saveRDS(test_imputed, "models/classifier/imputer/test_high_imputed.rds")
cv_splits_imputed <- impute_cv_splits(data_high$cv_splits, imputer_high)
saveRDS(
  cv_splits_imputed,
  "models/classifier/imputer/cv_splits_high_imputed.rds"
)


# read imputed data
rm(list = ls())
gc()
message("Reading imputed data...")
data_low <- list(
  train = readRDS("models/classifier/imputer/train_low_imputed.rds"),
  test = readRDS("models/classifier/imputer/test_low_imputed.rds"),
  cv_splits = readRDS("models/classifier/imputer/cv_splits_low_imputed.rds")
)
data_high <- list(
  train = readRDS("models/classifier/imputer/train_high_imputed.rds"),
  test = readRDS("models/classifier/imputer/test_high_imputed.rds"),
  cv_splits = readRDS("models/classifier/imputer/cv_splits_high_imputed.rds")
)

# Determine model features ----
model_features <- colnames(data_low$train)
model_features <- model_features[
  !model_features %in%
    c(
      "track.s.id",
      "artist.s.id",
      "metagenre"
    )
]
# Exclude SPotify distribution genres
# model_features <- model_features[!grepl("^dtb\\.", model_features)]

# Random Baseline Models ----
random_baseline_low <- train_random_baseline(
  data_low$train,
  data_low$test,
  list(seed = 42)
)
save_classification_model(
  random_baseline_low,
  "random_baseline_lowres",
  subfolder = "baseline"
)

random_baseline_high <- train_random_baseline(
  data_high$train,
  data_high$test,
  list(seed = 42)
)
save_classification_model(
  random_baseline_high,
  "random_baseline_highres",
  subfolder = "baseline"
)

# GLMNET Models ----
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
