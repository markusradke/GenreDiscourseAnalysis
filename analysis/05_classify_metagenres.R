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
rf_data_low <- prepare_rf_data(
  settings,
  poptrag,
  read_feather_with_lists("models/metagenres/mb_metagenres_10_15.feather")
)
compare_metagenre_distributions(rf_data_low)
rf_data_high <- prepare_rf_data(
  settings,
  poptrag,
  read_feather_with_lists("models/metagenres/mb_metagenres_25_30.feather")
)
compare_metagenre_distributions(rf_data_high)

saveRDS(settings, "models/classifier/rf_data_settings.rds")
saveRDS(rf_data_low, "models/classifier/rf_data_low.rds")
saveRDS(rf_data_high, "models/classifier/rf_data_high.rds")
# rm(list = ls())
# gc()

message("Reading data...")
rf_data_low <- readRDS("models/classifier/rf_data_low.rds")
rf_data_high <- readRDS("models/classifier/rf_data_high.rds")


# Impute missing values with missForestPredict ----
rf_data_imp_low <- train_and_impute_missing_data(
  rf_data_low,
  n_cores = 8,
  seed = 42,
  maxiter_imp = 1,
  max_rows_for_imputer = 50000,
  max_missing_prop_for_imputer = 0.3
)
saveRDS(
  rf_data_imp_low$imputer_model,
  "models/classifier/rf_imputer_model_low.rds"
)
saveRDS(rf_data_imp_low$datasets, "models/classifier/rf_data_low_imp.rds")

rf_data_imp_high <- train_and_impute_missing_data(
  rf_data_high,
  n_cores = 4,
  seed = 42,
  maxiter_imp = 1,
  max_rows_for_imputer = 50000,
  max_missing_prop_for_imputer = 0.3
)
saveRDS(
  rf_data_imp_high$imputer_model,
  "models/classifier/rf_imputer_model_high.rds"
)
saveRDS(rf_data_imp_high$datasets, "models/classifier/rf_data_high_imp.rds")

# Train models ----
# rf_data <- list(
#   low = readRDS("models/classifier/rf_data_low.rds"),
#   high = readRDS("models/classifier/rf_data_high.rds")
# )

# model_features <- colnames(rf_data$low$train)
# model_features <- model_features[
#   !model_features %in%
#     c(
#       "track.s.id",
#       "artist.s.id",
#       "metagenre"
#     )
# ]
# do not use Spotify genres as features
# model_features <- model_features[!grepl("^dtb\\.", model_features)]

# Baseline models ----
# settings <- list(
#   seed = 42,
#   n_cores = 8,
#   run_random_low = TRUE,
#   run_random_high = TRUE,
#   run_glmnet_low = TRUE, # Takes too long with that many features
#   run_glmnet_high = FALSE,
#   model_features = model_features,
#   glmnet_alpha = 0.5 # (0=ridge, 1=lasso, default=0.5)
# )
# baseline_res <- train_and_evaluate_baselines(settings, rf_data)

# # interpret baseline model
# detail <- "low"
# # detail <- "high"
# plot_cm(
#   baseline_res$glmnet[[detail]]$evaluation$train$confusion_matrix,
#   baseline_res$glmnet[[detail]]$evaluation$train$metrics
# )
# plot_cm(
#   baseline_res$glmnet[[detail]]$evaluation$test$confusion_matrix,
#   baseline_res$glmnet[[detail]]$evaluation$test$metrics
# )

# most_important_coefs <- broom::tidy(
#   baseline_res$glmnet[[detail]]$model$fit$fit
# ) |>
#   dplyr::filter(term != "(Intercept)") |>
#   dplyr::group_by(class) |>
#   dplyr::arrange(dplyr::desc(abs(estimate))) |>
#   dplyr::slice_head(n = 5) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(estimate = round(estimate, 2)) |>
#   dplyr::select(-penalty)
# flextable::flextable(most_important_coefs) |>
#   flextable::autofit() |>
#   flextable::merge_v(j = "class") |>
#   # horizontal line every 5 rows
#   flextable::hline(
#     i = seq(5, nrow(most_important_coefs), by = 5),
#     border = officer::fp_border(width = 1)
#   )

# Random forest models ----
# settings <- list(
#   seed = 42,
#   ntrees = 3, # make 1000 for final
#   ntrees_tuning = 1, # make 100 for now
#   under_ratio = 11, # irrelevant for tuning
#   n_cores = 32, # make large only for final
#   mtry = 72, # irrelevant for tuning
#   min.node.size = 50, # irrelevant for tuning
#   varimp_top_n = 40,
#   run_rf_low = TRUE,
#   run_rf_high = FALSE, # make seperate to spare memory
#   features_after_impute = model_features,
#   tune_hyperparameters = TRUE,
#   importance = "impurity", # better for tuning, final model is "permutation" by default
#   uncertain_jump = 5,
#   bayes_iterations = 2, # adjust later
#   initial_grid_density = 2 # adjust later
# )
# saveRDS(settings, "models/classifier/rf_tune_settings.rds")

# message("---TRAINING LOW RESOLUTION MODEL---")
# res_rf_low <- train_and_evaluate_rf_tidy(
#   settings,
#   rf_data
# )

# message("Saving model evaluation and settings...")
# saveRDS(res_rf_low$low$evaluation, "models/classifier/rf_mb_lowres_eval.rds")
# saveRDS(
#   res_rf_low$low$model_settings,
#   "models/classifier/rf_mb_lowres_settings.rds"
# )
# message("Saving tuning plots...")
# plot_tuning_results(res_rf_low$low$tuning_results)
# ggplot2::ggsave(
#   "models/classifier/rf_mb_lowres_tuning_parameters.png",
#   width = 20,
#   height = 8
# )
# plot_tuning_performance(res_rf_low$low$tuning_results, metric = "f_meas")
# ggplot2::ggsave(
#   "models/classifier/rf_mb_lowres_tuning_performance.png",
#   width = 8,
#   height = 6
# )
# message("Saving best model...")
# saveRDS(res_rf_low$low$model, "models/classifier/best_model_low.rds")

# message("---TRAINING HIGH RESOLUTION MODEL---")
# settings$run_rf_low <- FALSE
# settings$run_rf_high <- TRUE
# res_rf_high <- train_and_evaluate_rf_tidy(
#   settings,
#   rf_data
# )

# message("Saving model evaluation and settings...")
# saveRDS(res_rf_high$high$evaluation, "models/classifier/rf_mb_highres_eval.rds")
# saveRDS(
#   res_rf_high$high$model_settings,
#   "models/classifier/rf_mb_highres_settings.rds"
# )
# message("Saving tuning plots...")
# plot_tuning_results(res_rf_high$high$tuning_results)
# ggplot2::ggsave(
#   "models/classifier/rf_mb_highres_tuning_parameters.png",
#   width = 12,
#   height = 8
# )
# plot_tuning_performance(res_rf_high$high$tuning_results, metric = "f_meas")
# ggplot2::ggsave(
#   "models/classifier/rf_mb_highres_tuning_performance.png",
#   width = 8,
#   height = 6
# )
# message("Saving best model...")
# saveRDS(res_rf_high$high$model, "models/classifier/best_model_high.rds")

# # Generate resport ----
# generate_report("05_rf_classifier")
