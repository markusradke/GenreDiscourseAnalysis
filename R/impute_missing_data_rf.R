train_and_impute_missing_data <- function(
  dataset,
  n_cores = 1,
  seed = 42,
  maxiter_imp = 1
) {
  imputer <- NULL
  train <- dataset$train
  test <- dataset$test
  cv_splits <- dataset$cv_splits

  imputer <- train_imputer(
    train,
    nthreads = n_cores,
    seed = seed,
    maxiter = maxiter_imp
  )
  train_imputed <- imputer$ximp
  test_imputed <- apply_imputer(test, imputer)
  # TODO apply imputer to all cv splits analysis and assesment sets
}


train_imputer <- function(impute_frame, nthreads = 19, seed = 42, maxiter = 1) {
  set.seed(seed)
  message("---TRAINING MISSFOREST IMPUTER---")
  missForestPredict::missForest(
    as.data.frame(
      impute_frame |>
        dplyr::select(-track.s.id, -metagenre, -n_NA)
    ),
    maxiter = maxiter,
    mtry = floor(sqrt(ncol(impute_frame) - 3)),
    replace = TRUE,
    verbose = TRUE,
    num.threads = nthreads
  )
}

apply_imputer <- function(df, imputer_model) {
  imputed_data <- missForestPredict::missForestPredict(
    imputer_model,
    as.data.frame(
      df |>
        dplyr::select(-track.s.id, -metagenre, -n_NA)
    )
  )
  df |>
    dplyr::select(track.s.id, metagenre) |>
    dplyr::bind_cols(as.data.frame(imputed_data))
}
