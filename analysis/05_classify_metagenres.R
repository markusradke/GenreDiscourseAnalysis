rm(list = ls())
gc()
library(bonsai)
devtools::load_all()
options(tidymodels.dark = TRUE)
run_data_pre <- FALSE
run_glmnet <- FALSE
run_rda <- FALSE
run_lightgbm <- TRUE
run_rf <- FALSE
max_cores <- 64 # for final model fitting
max_cores_tuning <- 5 # for parallel tuning of GLMNET, MARS and RDA (multiple of n_folds, max n_folds x grid)
reserve_cores <- 4 # cores to leave free on the machine
n_folds <- 5
n_initial_grid <- 20
n_bayes_iter <- 50
checkpoint_chunk_size <- 1
enable_grid_checkpoints <- TRUE # Enable/disable grid phase checkpointing
enable_bayes_checkpoints <- TRUE # Enable/disable Bayesian phase checkpointing
process_low <- FALSE
process_medium <- FALSE
process_high <- TRUE
process_very_high <- TRUE

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
    subsample_prop = 1.0,
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
      read_feather_with_lists("models/metagenres/mb_metagenres_medium.feather")
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
# Exclude Spotify distribution genres
# model_features <- model_features[!grepl("^dtb\\.", model_features)]
# Only Spotify ditribution genres
# model_features <- model_features[grepl("^dtb\\.", model_features)]

# GLMNET Models ----
if (isTRUE(run_glmnet)) {
  settings <- list(
    seed = 42,
    model_features = model_features,
    n_cores = max_cores,
    n_cores_tuning = max_cores_tuning,
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
    rm(lightgbm_high, settings)
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
    rm(lightgbm_very_high, settings)
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
      rda_high,
      "rda_highres",
      subfolder = "rda",
      train_df = train_high,
      test_df = test_high
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

# Generate resport ----

# generate graphs for paper
# glmnet_low_tuning <- readRDS(
#   "models/classifier/glmnet/glmnet_lowres_436e205a_tuning_history.rds"
# )
# glmnet_low_settings <- readRDS(
#   "models/classifier/glmnet/glmnet_lowres_436e205a_settings.rds"
# )
# glmnet_medium_tuning <- readRDS(
#   "models/classifier/glmnet/glmnet_mediumres_01f3f5dd_tuning_history.rds"
# )
# glmnet_medium_settings <- readRDS(
#   "models/classifier/glmnet/glmnet_mediumres_01f3f5dd_settings.rds"
# )
# rda_low_tuning <- readRDS(
#   "models/classifier/rda/rda_lowres_d07f8bb4_tuning_history.rds"
# )
# rda_low_settings <- readRDS(
#   "models/classifier/rda/rda_lowres_d07f8bb4_settings.rds"
# )
# rda_medium_tuning <- readRDS(
#   "models/classifier/rda/rda_mediumres_4d480cd8_tuning_history.rds"
# )
# rda_medium_settings <- readRDS(
#   "models/classifier/rda/rda_mediumres_4d480cd8_settings.rds"
# )
# rf_low_tuning <- readRDS(
#   "models/classifier/rf/rf_lowres_759e2621_tuning_history.rds"
# )
# rf_low_settings <- readRDS(
#   "models/classifier/rf/rf_lowres_759e2621_settings.rds"
# )
# rf_medium_tuning <- readRDS(
#   "models/classifier/rf/rf_mediumres_2cc36c89_tuning_history.rds"
# )
# rf_medium_settings <- readRDS(
#   "models/classifier/rf/rf_mediumres_2cc36c89_settings.rds"
# )
# gbm_low_tuning <- readRDS(
#   "models/classifier/lightgbm/lightgbm_lowres_1776db28_tuning_history.rds"
# )
# gbm_low_settings <- readRDS(
#   "models/classifier/lightgbm/lightgbm_lowres_1776db28_settings.rds"
# )
# gbm_medium_tuning <- readRDS(
#   "models/classifier/lightgbm/lightgbm_mediumres_b8c05240_tuning_history.rds"
# )
# gbm_medium_settings <- readRDS(
#   "models/classifier/lightgbm/lightgbm_mediumres_b8c05240_settings.rds"
# )

# # get chosen model F1 macro cv mean and standard error for each model
# glmnet_chosen_low <- c(
#   penalty = glmnet_low_settings$penalty,
#   mixture = glmnet_low_settings$alpha,
#   target_ratio = glmnet_low_settings$target_ratio
# )
# glmnet_low_f1_cv <- prepare_tuning_topmodels(
#   glmnet_low_tuning,
#   glmnet_chosen_low,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "GLMNET", detail = "low")

# glmnet_chosen_medium <- c(
#   penalty = glmnet_medium_settings$penalty,
#   mixture = glmnet_medium_settings$alpha,
#   target_ratio = glmnet_medium_settings$target_ratio
# )
# glmnet_medium_f1_cv <- prepare_tuning_topmodels(
#   glmnet_medium_tuning,
#   glmnet_chosen_medium,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "GLMNET", detail = "medium")

# rda_chosen_low <- c(
#   gamma = rda_low_settings$gamma,
#   lambda = rda_low_settings$lambda,
#   target_ratio = rda_low_settings$target_ratio
# )
# rda_low_f1_cv <- prepare_tuning_topmodels(
#   rda_low_tuning,
#   rda_chosen_low,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "RDA", detail = "low")

# rda_chosen_medium <- c(
#   gamma = rda_medium_settings$gamma,
#   lambda = rda_medium_settings$lambda,
#   target_ratio = rda_medium_settings$target_ratio
# )
# rda_medium_f1_cv <- prepare_tuning_topmodels(
#   rda_medium_tuning,
#   rda_chosen_medium,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "RDA", detail = "medium")

# rf_chosen_low <- c(
#   mtry = rf_low_settings$mtry,
#   min_n = rf_low_settings$min_n,
#   max_depth = rf_low_settings$max_depth,
#   target_ratio = rf_low_settings$target_ratio
# )
# rf_low_f1_cv <- prepare_tuning_topmodels(
#   rf_low_tuning,
#   rf_chosen_low,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "RF", detail = "low")

# rf_chosen_medium <- c(
#   mtry = rf_medium_settings$mtry,
#   min_n = rf_medium_settings$min_n,
#   max_depth = rf_medium_settings$max_depth,
#   target_ratio = rf_medium_settings$target_ratio
# )
# rf_medium_f1_cv <- prepare_tuning_topmodels(
#   rf_medium_tuning,
#   rf_chosen_medium,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "RF", detail = "medium")

# gbm_chosen_low <- c(
#   tree_depth = gbm_low_settings$tree_depth,
#   trees = gbm_low_settings$trees,
#   learn_rate = gbm_low_settings$learn_rate,
#   mtry = gbm_low_settings$mtry,
#   min_n = gbm_low_settings$min_n,
#   target_ratio = gbm_low_settings$target_ratio
# )
# gbm_low_f1_cv <- prepare_tuning_topmodels(
#   gbm_low_tuning,
#   gbm_chosen_low,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "LightGBM", detail = "low")

# gbm_chosen_medium <- c(
#   tree_depth = gbm_medium_settings$tree_depth,
#   trees = gbm_medium_settings$trees,
#   learn_rate = gbm_medium_settings$learn_rate,
#   mtry = gbm_medium_settings$mtry,
#   min_n = gbm_medium_settings$min_n,
#   target_ratio = gbm_medium_settings$target_ratio
# )
# gbm_medium_f1_cv <- prepare_tuning_topmodels(
#   gbm_medium_tuning,
#   gbm_chosen_medium,
#   top_n = 100
# ) |>
#   dplyr::filter(is_chosen) |>
#   dplyr::select(
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(learner = "LightGBM", detail = "medium")

# chosen_models <- dplyr::bind_rows(
#   glmnet_low_f1_cv,
#   glmnet_medium_f1_cv,
#   rda_low_f1_cv,
#   rda_medium_f1_cv,
#   rf_low_f1_cv,
#   rf_medium_f1_cv,
#   gbm_low_f1_cv,
#   gbm_medium_f1_cv
# )

# # plot with detail on the x axis, macro f1 with error bars on the y axis, lightGBM is gree, all other learners grey, namespace ggplot notation
# label_df <- chosen_models |>
#   dplyr::distinct(
#     learner,
#     detail,
#     macro_f1_with_zeros_mean,
#     macro_f1_with_zeros_std_err
#   ) |>
#   dplyr::mutate(
#     color_group = ifelse(learner == "LightGBM", "LightGBM", "Other"),
#     y_pos = dplyr::case_when(
#       detail == "low" ~ 0.62,
#       detail == "medium" ~ 0.52,
#       TRUE ~ 0.45
#     )
#   )

# learner_comp_plot <- ggplot2::ggplot(
#   chosen_models,
#   ggplot2::aes(
#     x = detail,
#     y = macro_f1_with_zeros_mean,
#     color = ifelse(learner == "LightGBM", "LightGBM", "Other"),
#     group = learner
#   )
# ) +
#   ggplot2::geom_point(
#     position = ggplot2::position_dodge(width = 0.5),
#     size = 3
#   ) +
#   ggplot2::geom_errorbar(
#     ggplot2::aes(
#       ymin = macro_f1_with_zeros_mean - macro_f1_with_zeros_std_err,
#       ymax = macro_f1_with_zeros_mean + macro_f1_with_zeros_std_err,
#       color = ifelse(learner == "LightGBM", "LightGBM", "Other")
#     ),
#     width = 0.2,
#     position = ggplot2::position_dodge(width = 0.5)
#   ) +
#   ggplot2::geom_text(
#     data = label_df,
#     ggplot2::aes(
#       x = detail,
#       y = y_pos,
#       label = learner,
#       color = color_group
#     ),
#     position = ggplot2::position_dodge(width = 0.5),
#     vjust = 0.5,
#     size = 16 / ggplot2::.pt,
#     show.legend = FALSE,
#     angle = 45
#   ) +
#   ggplot2::scale_color_manual(
#     values = c("LightGBM" = "#3e578e", "Other" = "grey45")
#   ) +
#   ggplot2::scale_x_discrete(
#     breaks = c("low", "medium"),
#     labels = c("5 supergenres", "12 supergenres")
#   ) +
#   ggplot2::labs(
#     x = "",
#     y = "Macro F1 Score"
#   ) +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     legend.position = "none",
#     panel.grid.major.x = ggplot2::element_blank(),
#     panel.grid.minor.x = ggplot2::element_blank(),
#     axis.text.y = ggplot2::element_text(
#       size = 12,
#       color = "grey45",
#       face = "bold"
#     ),
#     axis.title.y = ggplot2::element_text(
#       size = 18,
#       color = "grey45",
#       hjust = 1
#     ),
#     axis.text.x = ggplot2::element_text(size = 18)
#   )
# ggplot2::ggsave(
#   "reports/paper_v1/figures/learner_comparison.png",
#   plot = learner_comp_plot,
#   width = 180,
#   height = 115,
#   units = "mm",
#   dpi = 300
# )
# generate_report("05_rf_classifier")
