orchestrate_classifier_training <- function(settings) {
  rm(list = ls()[!ls() %in% c("settings")])
  gc()
  options(tidymodels.dark = TRUE)

  # extract settings ----
  run_data_pre <- settings$run_data_pre
  subsample_prop <- settings$subsample_prop
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
      subsample_prop = subsample_prop,
      casewise_threshold = 0.4,
      artist_initial_split = 0.8,
      drop_POPULARMUSIC = TRUE,
      cv_folds = cv_folds,
      cv_repeats = cv_repeats,
      max_tracks_per_artist_cv = max_tracks_per_artist_cv,
      s_genremapping = s_genremapping,
      min_n_factor_level = 100
    )
    saveRDS(settings, "models/classifier/data_settings.rds")

    if (isTRUE(process_low)) {
      data_low <- prepare_classification_data(
        settings,
        poptrag,
        read_feather_with_lists("models/metagenres/mb_metagenres_low.feather")
      )
      saveRDS(data_low$train, "models/classifier/train_low.rds")
      saveRDS(data_low$test, "models/classifier/test_low.rds")
      saveRDS(data_low$cv_splits, "models/classifier/cv_splits_low.rds")
    }

    if (isTRUE(process_medium)) {
      data_medium <- prepare_classification_data(
        settings,
        poptrag,
        read_feather_with_lists(
          "models/metagenres/mb_metagenres_medium.feather"
        )
      )
      saveRDS(data_medium$train, "models/classifier/train_medium.rds")
      saveRDS(data_medium$test, "models/classifier/test_medium.rds")
      saveRDS(data_medium$cv_splits, "models/classifier/cv_splits_medium.rds")
    }

    if (isTRUE(process_high)) {
      data_high <- prepare_classification_data(
        settings,
        poptrag,
        read_feather_with_lists("models/metagenres/mb_metagenres_high.feather")
      )
      saveRDS(data_high$train, "models/classifier/train_high.rds")
      saveRDS(data_high$test, "models/classifier/test_high.rds")
      saveRDS(data_high$cv_splits, "models/classifier/cv_splits_high.rds")
    }

    if (isTRUE(process_very_high)) {
      data_very_high <- prepare_classification_data(
        settings,
        poptrag,
        read_feather_with_lists(
          "models/metagenres/mb_metagenres_very_high.feather"
        )
      )
      saveRDS(data_very_high$train, "models/classifier/train_very_high.rds")
      saveRDS(data_very_high$test, "models/classifier/test_very_high.rds")
      saveRDS(
        data_very_high$cv_splits,
        "models/classifier/cv_splits_very_high.rds"
      )
    }
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
          "enable_bayes_checkpoints",
          "process_low",
          "process_medium",
          "process_high",
          "process_very_high"
        )
    ]
  )
  gc()
  message("Reading training and test data...")

  # complete data
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
