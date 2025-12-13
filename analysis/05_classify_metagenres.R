rm(list = ls())
gc()
library(bonsai)
devtools::load_all()
options(tidymodels.dark = TRUE)
run_data_pre <- FALSE
run_baseline <- FALSE
run_glmnet <- FALSE
run_rda <- TRUE
run_rf <- FALSE
run_lightgbm <- TRUE
max_cores <- 64 # for final model fitting
max_cores_tuning <- 5 # for parallel tuning of GLMNET, MARS and RDA (multiple of n_folds, max n_folds x grid)
reserve_cores <- 4 # cores to leave free on the machine
n_folds <- 5
n_initial_grid <- 10
n_bayes_iter <- 15
checkpoint_chunk_size <- 1
enable_grid_checkpoints <- TRUE # Enable/disable grid phase checkpointing
enable_bayes_checkpoints <- TRUE # Enable/disable Bayesian phase checkpointing

if (isTRUE(run_data_pre)) {
  poptrag <- readRDS("data-raw/poptrag.rds")
  s_genremapping <- readRDS(
    "models/metagenres/tune_s_metagenres.rds"
  )$solutions[[
    "1050"
  ]]$mapping
  cv_folds <- n_folds
  cv_repeats <- 1
  max_tracks_per_artist_cv <- 10000

  # # Prepare data sets for modeling ----
  settings <- list(
    seed = 42,
    subsample_prop = 1,
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
    read_feather_with_lists("models/metagenres/mb_metagenres_low.feather")
  )

  data_medium <- prepare_classification_data(
    settings,
    poptrag,
    read_feather_with_lists("models/metagenres/mb_metagenres_medium.feather")
  )

  data_high <- prepare_classification_data(
    settings,
    poptrag,
    read_feather_with_lists("models/metagenres/mb_metagenres_high.feather")
  )

  saveRDS(settings, "models/classifier/data_settings.rds")
  saveRDS(data_low$train, "models/classifier/train_low.rds")
  saveRDS(data_low$test, "models/classifier/test_low.rds")
  saveRDS(data_low$cv_splits, "models/classifier/cv_splits_low.rds")
  saveRDS(data_medium$train, "models/classifier/train_medium.rds")
  saveRDS(data_medium$test, "models/classifier/test_medium.rds")
  saveRDS(data_medium$cv_splits, "models/classifier/cv_splits_medium.rds")
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
        "run_rda",
        "reserve_cores",
        "n_initial_grid",
        "n_bayes_iter",
        "run_rf",
        "run_lightgbm",
        "checkpoint_chunk_size",
        "max_cores",
        "max_cores_tuning",
        "n_folds",
        "enable_grid_checkpoints",
        "enable_bayes_checkpoints"
      )
  ]
)
gc()
message("Reading training and test data...")

# complete data
cv_splits_low <- readRDS("models/classifier/cv_splits_low.rds")
train_low <- readRDS("models/classifier/train_low.rds")
test_low <- readRDS("models/classifier/test_low.rds")

cv_splits_medium <- readRDS("models/classifier/cv_splits_medium.rds")
train_medium <- readRDS("models/classifier/train_medium.rds")
test_medium = readRDS("models/classifier/test_medium.rds")

cv_splits_high <- readRDS("models/classifier/cv_splits_high.rds")
train_high <- readRDS("models/classifier/train_high.rds")
test_high = readRDS("models/classifier/test_high.rds")

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
message("--MODEL FEATURES--")
print(model_features)
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

  random_baseline_medium <- train_random_baseline(
    train_medium,
    test_medium,
    list(seed = 42)
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
  rm(random_baseline_low, random_baseline_high)
  gc()
}

# GLMNET Models ----
if (isTRUE(run_glmnet)) {
  settings <- list(
    seed = 42,
    model_features = model_features,
    n_cores = max_cores,
    n_cores_tuning = 5, #max_cores_tuning,
    use_caseweights = FALSE,
    tune_penalty = TRUE,
    tune_alpha = TRUE,
    tune_sampling = TRUE,
    penalty_fix = 0.002,
    alpha_fix = 0.5,
    target_ratio_fix = 5,
    initial_grid_size = n_initial_grid,
    bayes_iterations = n_bayes_iter,
    grid_chunk_size = checkpoint_chunk_size,
    uncertain_jump = 5,
    reserve_cores = reserve_cores,
    enable_grid_checkpoints = enable_grid_checkpoints,
    enable_bayes_checkpoints = enable_bayes_checkpoints
  )

  message("---TRAINING LOW RESOLUTION MODEL---")
  glmnet_low <- train_glmnet(train_low, test_low, cv_splits_low, settings)

  save_classification_model(
    glmnet_low,
    "glmnet_lowres",
    subfolder = "glmnet",
    train_df = train_low,
    test_df = test_low
  )
  rm(glmnet_low)
  gc()

  message("---TRAINING MEDIUM RESOLUTION MODEL---")
  settings$target_ratio_fix <- 7
  glmnet_medium <- train_glmnet(
    train_medium,
    test_medium,
    cv_splits_medium,
    settings
  )
  save_classification_model(
    glmnet_medium,
    "glmnet_mediumres",
    subfolder = "glmnet",
    train_df = train_medium,
    test_df = test_medium
  )
  rm(glmnet_medium)
  gc()

  message("---TRAINING HIGH RESOLUTION MODEL---")
  settings$target_ratio_fix <- 10
  glmnet_high <- train_glmnet(train_high, test_high, cv_splits_high, settings)
  save_classification_model(
    glmnet_high,
    "glmnet_highres",
    subfolder = "glmnet",
    train_df = train_high,
    test_df = test_high
  )
  rm(glmnet_high, settings)
  gc()
}

if (isTRUE(run_rf)) {
  # Random forest models ----
  settings <- list(
    seed = 42,
    ntrees = 1000,
    n_cores = max_cores,
    n_cores_tuning = 1, #5,
    varimp_top_n = 40,
    model_features = model_features,
    importance = "impurity",
    use_caseweights = FALSE,
    tune_mtry = TRUE,
    tune_min_n = TRUE,
    tune_max_depth = TRUE,
    tune_sampling = TRUE,
    ntrees_tuning = 500,
    initial_grid_size = n_initial_grid,
    bayes_iterations = n_bayes_iter,
    uncertain_jump = 5,
    grid_chunk_size = checkpoint_chunk_size,
    min.node.size_fix = 50,
    max.depth_fix = Inf,
    mtry_fix = 11,
    target_ratio_fix = 5,
    reserve_cores = reserve_cores,
    enable_grid_checkpoints = enable_grid_checkpoints,
    enable_bayes_checkpoints = enable_bayes_checkpoints
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
  rm("rf_low")
  gc()

  message("---TRAINING HIGH RESOLUTION MODEL---")
  settings$target_ratio_fix <- 10
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

#LightGBM Models ----
if (isTRUE(run_lightgbm)) {
  settings <- list(
    seed = 42,
    model_features = model_features,
    n_cores = max_cores,
    n_cores_tuning = max_cores_tuning,
    use_caseweights = FALSE,
    tune_tree_depth = TRUE,
    tune_trees = TRUE,
    tune_learn_rate = TRUE,
    tune_mtry = TRUE,
    tune_min_n = TRUE,
    tune_sampling = TRUE,
    tree_depth_fix = 6,
    trees_fix = 100,
    learn_rate_fix = 0.1,
    mtry_fix = NULL,
    min_n_fix = 20,
    target_ratio_fix = 5,
    initial_grid_size = n_initial_grid,
    bayes_iterations = n_bayes_iter,
    grid_chunk_size = checkpoint_chunk_size,
    uncertain_jump = 5,
    reserve_cores = reserve_cores,
    enable_grid_checkpoints = enable_grid_checkpoints,
    enable_bayes_checkpoints = enable_bayes_checkpoints
  )

  message("---TRAINING LOW RESOLUTION MODEL---")
  lightgbm_low <- train_gbm(
    train_low,
    test_low,
    cv_splits_low,
    settings
  )
  save_classification_model(
    lightgbm_low,
    "lightgbm_lowres",
    subfolder = "lightgbm",
    train_df = train_low,
    test_df = test_low
  )
  rm(lightgbm_low)
  gc()

  message("---TRAINING MEDIUM RESOLUTION MODEL---")
  settings$target_ratio_fix <- 7
  lightgbm_medium <- train_gbm(
    train_medium,
    test_medium,
    cv_splits_medium,
    settings
  )
  save_classification_model(
    lightgbm_medium,
    "lightgbm_mediumres",
    subfolder = "lightgbm",
    train_df = train_medium,
    test_df = test_medium
  )
  rm(lightgbm_medium)
  gc()

  # message("---TRAINING HIGH RESOLUTION MODEL---")
  # settings$under_ratio_fix <- 20
  # lightgbm_high <- train_gbm(
  #   train_high,
  #   test_high,
  #   cv_splits_high,
  #   settings
  # )
  # save_classification_model(
  #   lightgbm_high,
  #   "lightgbm_highres",
  #   subfolder = "lightgbm",
  #   train_df = train_high,
  #   test_df = test_high
  # )
  # rm(lightgbm_high, settings)
  # gc()
}

if (isTRUE(run_rda)) {
  # RDA Models ----
  library(discrim)
  settings <- list(
    seed = 42,
    model_features = model_features,
    n_cores = max_cores,
    n_cores_tuning = max_cores_tuning,
    use_caseweights = FALSE,
    tune_gamma = TRUE,
    tune_lambda = TRUE,
    tune_sampling = TRUE,
    gamma_fix = 0.2,
    lambda_fix = 0.8,
    target_ratio_fix = 5,
    initial_grid_size = n_initial_grid,
    bayes_iterations = n_bayes_iter,
    grid_chunk_size = checkpoint_chunk_size,
    uncertain_jump = 5,
    reserve_cores = reserve_cores,
    enable_grid_checkpoints = enable_grid_checkpoints,
    enable_bayes_checkpoints = enable_bayes_checkpoints
  )
  message("---TRAINING LOW RESOLUTION MODEL---")
  rda_low <- train_rda(
    train_low,
    test_low,
    cv_splits_low,
    settings
  )
  save_classification_model(
    rda_low,
    "rda_lowres",
    subfolder = "rda",
    train_df = train_low,
    test_df = test_low
  )
  rm("rda_low")
  gc()
  message("---TRAINING MEDIUM RESOLUTION MODEL---")
  settings$target_ratio_fix <- 7
  rda_medium <- train_rda(
    train_medium,
    test_medium,
    cv_splits_medium,
    settings
  )
  save_classification_model(
    rda_high,
    "rda_highres",
    subfolder = "rda",
    train_df = train_high,
    test_df = test_high
  )
  rm("rda_medium")
  gc()
  # message("---TRAINING HIGH RESOLUTION MODEL---")
  # settings$under_ratio_fix <- 20
  # rda_high <- train_rda(
  #   train_high,
  #   test_high,
  #   cv_splits_high,
  #   settings
  # )
  # save_classification_model(
  #   rda_high,
  #   "rda_highres",
  #   subfolder = "rda",
  #   train_df = train_high,
  #   test_df = test_high
  # )
}

# Generate resport ----
# generate_report("05_rf_classifier")
