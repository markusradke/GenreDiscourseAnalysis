orchestrate_classifier_training <- function(settings) {
  rm(list = ls()[!ls() %in% c("settings")])
  gc()
  options(tidymodels.dark = TRUE)

  # extract settings ----
  run_data_pre <- settings$run_data_pre
  subsample_prop <- settings$train_subsample_prop
  run_glmnet <- settings$run_glmnet
  run_rda <- settings$run_rda
  run_rf <- settings$run_rf
  run_lightgbm <- settings$run_lightgbm
  max_cores <- settings$max_cores
  reserve_cores <- settings$reserve_cores
  n_folds <- settings$n_folds
  n_initial_grid <- settings$n_initial_grid
  n_bayes_iter <- settings$n_bayes_iter
  process_low <- settings$process_low
  process_medium <- settings$process_medium
  process_high <- settings$process_high
  process_very_high <- settings$process_very_high

  message("Reading training and test data...")

  if (isTRUE(process_low)) {
    cv_splits_low <- readRDS("models/classifier/cv_splits_low.rds")
    train_low <- readRDS("models/classifier/train_low.rds")
    test_low <- readRDS("models/classifier/test_low.rds")
    featureset <- colnames(train_low)
  }

  if (isTRUE(process_medium)) {
    cv_splits_medium <- readRDS("models/classifier/cv_splits_medium.rds")
    train_medium <- readRDS("models/classifier/train_medium.rds")
    test_medium <- readRDS("models/classifier/test_medium.rds")
    featureset <- colnames(train_medium)
  }

  if (isTRUE(process_high)) {
    cv_splits_high <- readRDS("models/classifier/cv_splits_high.rds")
    train_high <- readRDS("models/classifier/train_high.rds")
    test_high <- readRDS("models/classifier/test_high.rds")
    featureset <- colnames(train_high)
  }

  if (isTRUE(process_very_high)) {
    cv_splits_very_high <- readRDS("models/classifier/cv_splits_very_high.rds")
    train_very_high <- readRDS("models/classifier/train_very_high.rds")
    test_very_high <- readRDS("models/classifier/test_very_high.rds")
    featureset <- colnames(train_very_high)
  }

  if (subsample_prop < 1) {
    message(
      "Subsampling full train data and generating new cv_splits for prototyping..."
    )
    subsample_full_data_for_prototyping <- function(train, subsample_prop) {
      train_subsampled <- draw_prototype_sample(train, subsample_prop)
      splits_subsampled <- create_artist_cv_splits(
        train_data = train_subsampled,
        n_folds = n_folds,
        max_tracks_per_artist = 2000 # 10k * 0.2
      )
      list(
        train = train_subsampled,
        cv_splits = splits_subsampled
      )
    }

    if (isTRUE(process_low)) {
      low_list <- subsample_full_data_for_prototyping(
        train_low,
        subsample_prop
      )
      train_low <- low_list$train
      cv_splits_low <- low_list$cv_splits
      rm(low_list)
    }
    if (isTRUE(process_medium)) {
      medium_list <- subsample_full_data_for_prototyping(
        train_medium,
        subsample_prop
      )
      train_medium <- medium_list$train
      cv_splits_medium <- medium_list$cv_splits
      rm(medium_list)
    }
    if (isTRUE(process_high)) {
      high_list <- subsample_full_data_for_prototyping(
        train_high,
        subsample_prop
      )
      train_high <- high_list$train
      cv_splits_high <- high_list$cv_splits
      rm(high_list)
    }
    if (isTRUE(process_very_high)) {
      very_high_list <- subsample_full_data_for_prototyping(
        train_very_high,
        subsample_prop
      )
      train_very_high <- very_high_list$train
      cv_splits_very_high <- very_high_list$cv_splits
      rm(very_high_list)
    }
    gc()
  }

  # Determine model features ----

  model_features <- featureset[
    !featureset %in%
      c(
        "track.s.id",
        "artist.s.id",
        "metagenre",
        "case_wts"
      )
  ]
  message("--MODEL FEATURES--")
  print(model_features)

  # GLMNET Models ----
  if (isTRUE(run_glmnet)) {
    settings <- list(
      seed = 42,
      model_features = model_features,
      n_cores = max_cores,
      n_cores_tuning = 1, # when tuning sampling
      use_caseweights = FALSE,
      tune_penalty = TRUE,
      tune_alpha = TRUE,
      tune_sampling = TRUE,
      penalty_fix = 0.002,
      alpha_fix = 0.5,
      target_ratio_fix = 5,
      initial_grid_size = n_initial_grid,
      bayes_iterations = n_bayes_iter,
      chunk_size = 1,
      uncertain_jump = 5,
      reserve_cores = reserve_cores,
      enable_grid_checkpoints = TRUE,
      enable_bayes_checkpoints = TRUE
    )

    if (isTRUE(process_low)) {
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
    }

    if (isTRUE(process_medium)) {
      message("---TRAINING MEDIUM RESOLUTION MODEL---")
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
    }
  }

  #LightGBM Models ----
  if (isTRUE(run_lightgbm)) {
    library(bonsai)
    settings <- list(
      seed = 42,
      model_features = model_features,
      n_cores = max_cores,
      n_cores_tuning = 1, # when tuning sampling
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
      chunk_size = 1,
      uncertain_jump = 5,
      reserve_cores = reserve_cores,
      enable_grid_checkpoints = TRUE,
      enable_bayes_checkpoints = TRUE
    )

    if (isTRUE(process_low)) {
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
    }

    if (isTRUE(process_medium)) {
      message("---TRAINING MEDIUM RESOLUTION MODEL---")
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
    }

    if (isTRUE(process_high)) {
      message("---TRAINING HIGH RESOLUTION MODEL---")
      lightgbm_high <- train_gbm(
        train_high,
        test_high,
        cv_splits_high,
        settings
      )
      save_classification_model(
        lightgbm_high,
        "lightgbm_highres",
        subfolder = "lightgbm",
        train_df = train_high,
        test_df = test_high
      )
      rm(lightgbm_high)
      gc()
    }

    if (isTRUE(process_very_high)) {
      message("---TRAINING VERY HIGH RESOLUTION MODEL---")
      lightgbm_very_high <- train_gbm(
        train_very_high,
        test_very_high,
        cv_splits_very_high,
        settings
      )
      save_classification_model(
        lightgbm_very_high,
        "lightgbm_veryhighres",
        subfolder = "lightgbm",
        train_df = train_very_high,
        test_df = test_very_high
      )
      rm(lightgbm_very_high)
      gc()
    }
  }

  if (isTRUE(run_rda)) {
    # RDA Models ----
    library(discrim)
    settings <- list(
      seed = 42,
      model_features = model_features,
      n_cores = max_cores,
      n_cores_tuning = 1, # when tuning sampling
      use_caseweights = FALSE,
      tune_gamma = TRUE,
      tune_lambda = TRUE,
      tune_sampling = TRUE,
      gamma_fix = 0.2,
      lambda_fix = 0.8,
      target_ratio_fix = 5,
      initial_grid_size = n_initial_grid,
      bayes_iterations = n_bayes_iter,
      chunk_size = 1,
      uncertain_jump = 5,
      reserve_cores = reserve_cores,
      enable_grid_checkpoints = TRUE,
      enable_bayes_checkpoints = TRUE
    )

    if (isTRUE(process_low)) {
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
    }

    if (isTRUE(process_medium)) {
      message("---TRAINING MEDIUM RESOLUTION MODEL---")
      rda_medium <- train_rda(
        train_medium,
        test_medium,
        cv_splits_medium,
        settings
      )
      save_classification_model(
        rda_medium,
        "rda_mediumres",
        subfolder = "rda",
        train_df = train_medium,
        test_df = test_medium
      )
      rm("rda_medium")
      gc()
    }
  }

  if (isTRUE(run_rf)) {
    # Random forest models ----
    settings <- list(
      seed = 42,
      ntrees = 1000,
      n_cores = max_cores,
      n_cores_tuning = 1, # when tuning sampling
      varimp_top_n = 40,
      model_features = model_features,
      importance = "permutation",
      use_caseweights = FALSE,
      tune_mtry = TRUE,
      tune_min_n = TRUE,
      tune_max_depth = TRUE,
      tune_sampling = TRUE,
      ntrees_tuning = 500,
      initial_grid_size = n_initial_grid,
      bayes_iterations = n_bayes_iter,
      uncertain_jump = 5,
      chunk_size = 1,
      min.node.size_fix = 50,
      max.depth_fix = Inf,
      mtry_fix = 11,
      target_ratio_fix = 5,
      reserve_cores = reserve_cores,
      enable_grid_checkpoints = TRUE,
      enable_bayes_checkpoints = TRUE
    )
    saveRDS(settings, "models/classifier/rf_tune_settings.rds")

    if (isTRUE(process_low)) {
      message("---TRAINING LOW RESOLUTION MODEL---")
      rf_low <- train_random_forest(
        train_low,
        test_low,
        cv_splits_low,
        settings
      )
      save_classification_model(
        rf_low,
        "rf_lowres",
        subfolder = "rf",
        train_df = train_low,
        test_df = test_low
      )
      rm("rf_low")
      gc()
    }

    if (isTRUE(process_medium)) {
      message("---TRAINING MEDIUM RESOLUTION MODEL---")
      rf_medium <- train_random_forest(
        train_medium,
        test_medium,
        cv_splits_medium,
        settings
      )
      save_classification_model(
        rf_medium,
        "rf_mediumres",
        subfolder = "rf",
        train_df = train_low,
        test_df = test_low
      )
      rm("rf_medium")
      gc()
    }
  }
}
