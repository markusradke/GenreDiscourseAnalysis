# prototype learners ----
rm(list = ls())
devtools::load_all()
# corrected settings list
settings <- list(
  run_data_pre = TRUE,
  subsample_prop = 0.2,
  run_glmnet = TRUE,
  run_rda = TRUE,
  run_rf = TRUE,
  run_lightgbm = TRUE,
  max_cores = 64,
  reserve_cores = 4,
  n_folds = 5,
  n_initial_grid = 10,
  n_bayes_iter = 15,
  process_low = TRUE,
  process_medium = TRUE,
  process_high = FALSE,
  process_very_high = FALSE
)

orchestrate_classifier_training(settings)

# train final models ----
rm(list = ls())
settings <- list(
  run_data_pre = TRUE,
  subsample_prop = 1,
  run_glmnet = FALSE,
  run_rda = FALSE,
  run_rf = FALSE,
  run_lightgbm = TRUE,
  max_cores = 64,
  reserve_cores = 4,
  n_folds = 5,
  n_initial_grid = 20,
  n_bayes_iter = 50,
  process_low = TRUE,
  process_medium = TRUE,
  process_high = TRUE,
  process_very_high = TRUE
)
orchestrate_classifier_training(settings)


# Generate resport ----
generate_report("05_classify_metagenres")
