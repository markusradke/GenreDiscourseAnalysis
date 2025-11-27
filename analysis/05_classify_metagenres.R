rm(list = ls())
gc()
devtools::load_all()
options(tidymodels.dark = TRUE)
run_data_pre <- FALSE
run_baseline <- FALSE
run_glmnet <- FALSE
run_rf <- TRUE
max_cores <- 64 # for final model fitting
max_cores_tuning <- 10 # for parallel tuning of GLMNET (multiple of n_folds, max n_folds x grid)
n_folds <- 5

if (isTRUE(run_data_pre)) {
  poptrag <- readRDS("data-raw/poptrag.rds")
  s_genremapping <- readRDS(
    "models/metagenres/tune_s_metagenres.rds"
  )$solutions[[
    "1050"
  ]]$mapping
  cv_folds <- 5
  cv_repeats <- 1
  max_tracks_per_artist_cv <- 10000

  # # Prepare data sets for modeling ----
  settings <- list(
    seed = 42,
    subsample_prop = 0.1,
    casewise_threshold = 0.4,
    artist_initial_split = 0.8,
    drop_POPULARMUSIC = TRUE,
    cv_folds = cv_folds,
    cv_repeats = cv_repeats,
    max_tracks_per_artist_cv = max_tracks_per_artist_cv,
    s_genremapping = s_genremapping,
    min_n_factor_level = 100
  )
  data_low <- prepare_classification_data(
    settings,
    poptrag,
    read_feather_with_lists("models/metagenres/mb_metagenres_10_15.feather")
  )

  data_high <- prepare_classification_data(
    settings,
    poptrag,
    read_feather_with_lists("models/metagenres/mb_metagenres_25_30.feather")
  )

  saveRDS(settings, "models/classifier/data_settings.rds")
  saveRDS(data_low$train, "models/classifier/train_low.rds")
  saveRDS(data_low$test, "models/classifier/test_low.rds")
  saveRDS(data_low$cv_splits, "models/classifier/cv_splits_low.rds")
  saveRDS(data_high$train, "models/classifier/train_high.rds")
  saveRDS(data_high$test, "models/classifier/test_high.rds")
  saveRDS(data_high$cv_splits, "models/classifier/cv_splits_high.rds")

  # saveRDS(settings, "models/classifier/mock_data_settings.rds")
  # saveRDS(data_low$train, "models/classifier/mock_train_low.rds")
  # saveRDS(data_low$test, "models/classifier/mock_test_low.rds")
  # saveRDS(data_low$cv_splits, "models/classifier/mock_cv_splits_low.rds")
  # saveRDS(data_high$train, "models/classifier/mock_train_high.rds")
  # saveRDS(data_high$test, "models/classifier/mock_test_high.rds")
  # saveRDS(data_high$cv_splits, "models/classifier/mock_cv_splits_high.rds")
}

# read in data
rm(
  list = ls()[
    !ls() %in%
      c(
        "run_baseline",
        "run_glmnet",
        "run_rf",
        "max_cores",
        "max_cores_tuning",
        "n_folds"
      )
  ]
)
gc()
message("Reading training and test data...")

# complete data
cv_splits_low <- readRDS("models/classifier/cv_splits_low.rds")
train_low <- readRDS("models/classifier/train_low.rds")
test_low <- readRDS("models/classifier/test_low.rds")

cv_splits_high <- readRDS("models/classifier/cv_splits_high.rds")
train_high <- readRDS("models/classifier/train_high.rds")
test_high = readRDS("models/classifier/test_high.rds")

# mock data for testing
# cv_splits_low <- readRDS("models/classifier/mock_cv_splits_low.rds")
# train_low <- readRDS("models/classifier/mock_train_low.rds")
# test_low <- readRDS("models/classifier/mock_test_low.rds")

# cv_splits_high <- readRDS("models/classifier/mock_cv_splits_high.rds")
# train_high <- readRDS("models/classifier/mock_train_high.rds")
# test_high = readRDS("models/classifier/mock_test_high.rds")

# Determine model features ----
model_features <- colnames(train_low)[
  !colnames(train_low) %in%
    c(
      "track.s.id",
      "artist.s.id",
      "metagenre",
      "case_wts"
    )
]
# Exclude Spotify distribution genres
# model_features <- model_features[!grepl("^dtb\\.", model_features)]
# Only Spotify ditribution genres
# model_features <- model_features[grepl("^dtb\\.", model_features)]

# Random Baseline Models ----
if (isTRUE(run_baseline)) {
  random_baseline_low <- train_random_baseline(
    train_low,
    test_low,
    list(seed = 42)
  )
  save_classification_model(
    random_baseline_low,
    "random_baseline_lowres",
    subfolder = "baseline",
    train_df = train_low,
    test_df = test_low
  )

  random_baseline_high <- train_random_baseline(
    train_high,
    test_high,
    list(seed = 42)
  )
  save_classification_model(
    random_baseline_high,
    "random_baseline_highres",
    subfolder = "baseline",
    train_df = train_high,
    test_df = test_high
  )
}

# GLMNET Models ----
if (isTRUE(run_glmnet)) {
  settings <- list(
    seed = 42,
    model_features = model_features,
    n_cores = max_cores,
    n_cores_tuning = max_cores_tuning,
    use_caseweights = FALSE,
    tune_penalty = TRUE,
    tune_alpha = FALSE,
    tune_downsample = TRUE,
    tune_upsample = TRUE,
    penalty_fix = 0.002,
    alpha_fix = 0.5,
    under_ratio_fix = 10,
    over_ratio_fix = 0.5,
    initial_grid_size = 20,
    bayes_iterations = 50,
    uncertain_jump = 5
  )

  glmnet_low <- train_glmnet(train_low, test_low, cv_splits_low, settings)

  save_classification_model(
    glmnet_low,
    "glmnet_lowres",
    subfolder = "glmnet",
    train_df = train_low,
    test_df = test_low
  )

  # display beta features of glmnet in a data frame

  glmnet_high <- train_glmnet(train_high, test_high, cv_splits_high, settings)
  save_classification_model(
    glmnet_high,
    "glmnet_highres",
    subfolder = "glmnet",
    train_df = train_high,
    test_df = test_high
  )
}

if (isTRUE(run_rf)) {
  # Random forest models ----
  settings <- list(
    seed = 42,
    ntrees = 1000, # make 1000 for final
    n_cores = max_cores,
    n_cores_tuning = n_folds, # no of workers (gets multiple threads determined by n_cores)
    varimp_top_n = 40,
    model_features = model_features,
    importance = "impurity", # better for tuning, final model is "permutation" by default
    use_caseweights = FALSE,
    tune_mtry = TRUE,
    tune_min_n = TRUE,
    tune_max_depth = TRUE,
    tune_downsample = TRUE,
    tune_upsample = TRUE,
    ntrees_tuning = 750, # make 750 later
    initial_grid_size = 2, # adjust later
    bayes_iterations = 2, # adjust later
    uncertain_jump = 5,
    min.node.size_fix = 50,
    max.depth_fix = Inf,
    mtry_fix = 11,
    under_ratio_fix = 11,
    over_ratio_fix = 0.5
  )
  saveRDS(settings, "models/classifier/rf_tune_settings.rds")

  message("---TRAINING LOW RESOLUTION MODEL---")
  rf_low <- train_random_forest(
    train_low,
    test_low,
    cv_splits_low,
    settings
  )
  # curve <- get_oob_error_curve(
  #   rf_low$model,
  #   mode = "classification",
  #   train_data = train_low |> dplyr::rename("outcome" = "metagenre")
  # )
  save_classification_model(
    rf_low,
    "rf_lowres",
    subfolder = "rf",
    train_df = train_low,
    test_df = test_low
  )

  message("---TRAINING HIGH RESOLUTION MODEL---")
  rf_high <- train_random_forest(
    train_high,
    test_high,
    cv_splits_high,
    settings
  )
  save_classification_model(
    rf_high,
    "rf_highres",
    subfolder = "rf",
    train_df = train_high,
    test_df = test_high
  )
}

# Generate resport ----
# generate_report("05_rf_classifier")
