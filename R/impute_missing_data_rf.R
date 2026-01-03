train_imputer <- function(
  impute_frame,
  max_rows = 50000,
  max_missing_prop = 0.3,
  nthreads = 4,
  seed = 42,
  maxiter = 1
) {
  set.seed(seed)
  message("---TRAINING MISSFOREST IMPUTER---")

  features <- get_features_for_imputation(impute_frame)
  missing_prop <- rowMeans(is.na(features))
  complete_enough <- missing_prop <= max_missing_prop

  message(sprintf(
    "Rows with less or equal %.0f%% missing: %d / %d",
    max_missing_prop * 100,
    sum(complete_enough),
    nrow(features)
  ))

  candidate_frame <- impute_frame[complete_enough, ]

  if (nrow(candidate_frame) > max_rows) {
    message(sprintf(
      "Stratified sampling by metagenre to %d rows",
      max_rows
    ))
    sampled_frame <- candidate_frame |>
      dplyr::group_by(metagenre) |>
      dplyr::slice_sample(
        prop = max_rows / nrow(candidate_frame),
        replace = FALSE
      ) |>
      dplyr::ungroup()
  } else {
    sampled_frame <- candidate_frame
  }

  features_sample <- get_features_for_imputation(sampled_frame)

  message(sprintf(
    "Training imputer on %d rows (%.1f%% avg missingness)",
    nrow(features_sample),
    100 * mean(rowMeans(is.na(features_sample)))
  ))

  missForestPredict::missForest(
    as.data.frame(features_sample),
    maxiter = maxiter,
    mtry = floor(sqrt(ncol(features_sample))),
    replace = TRUE,
    verbose = TRUE,
    num.threads = nthreads
  )
}


impute_cv_splits <- function(cv_splits, imputer) {
  message("---IMPUTING CV SPLITS---")
  splits_list <- lapply(
    seq_len(nrow(cv_splits)),
    function(i) impute_rsplit(cv_splits$splits[[i]], imputer)
  )
  rebuild_cv_splits(cv_splits, splits_list)
}

impute_rsplit <- function(split_obj, imputer) {
  data_imputed <- impute_data(split_obj$data, imputer)
  rsample::make_splits(
    x = list(
      analysis = split_obj$in_id,
      assessment = split_obj$out_id
    ),
    data = data_imputed
  )
}

impute_data <- function(df, imputer) {
  features <- get_features_for_imputation(df)
  imputed_features <- missForestPredict::missForestPredict(
    imputer,
    as.data.frame(features)
  )
  bind_imputed_with_metadata(df, imputed_features)
}

get_features_for_imputation <- function(df) {
  df |>
    dplyr::select(
      -dplyr::any_of(c("track.s.id", "artist.s.id", "metagenre", "case_wts"))
    )
}

bind_imputed_with_metadata <- function(df, imputed) {
  df |>
    dplyr::select(track.s.id, case_wts, artist.s.id, metagenre) |>
    dplyr::bind_cols(as.data.frame(imputed))
}


recreate_splits <- function(cv_splits, training_df) {
  message("---RECREATING CV SPLITS---")
  if (!"track.s.id" %in% names(training_df)) {
    stop("training_df must contain a column named 'track.s.id'")
  }

  new_splits <- lapply(seq_len(nrow(cv_splits)), function(i) {
    split_obj <- cv_splits$splits[[i]]
    orig_data <- split_obj$data

    if (!"track.s.id" %in% names(orig_data)) {
      stop(sprintf("split %d data does not contain 'track.s.id'", i))
    }

    analysis_ids <- orig_data$track.s.id[split_obj$in_id]
    assessment_ids <- orig_data$track.s.id[split_obj$out_id]

    analysis_idx <- match(analysis_ids, training_df$track.s.id)
    assessment_idx <- match(assessment_ids, training_df$track.s.id)

    missing_analysis <- sum(is.na(analysis_idx))
    missing_assessment <- sum(is.na(assessment_idx))

    if (missing_analysis > 0 || missing_assessment > 0) {
      stop(
        sprintf(
          "Missing ids in training_df for split %d: %d analysis, %d assessment (ensure training_df contains all track.s.id values)",
          i,
          missing_analysis,
          missing_assessment
        )
      )
    }

    rsample::make_splits(
      x = list(analysis = analysis_idx, assessment = assessment_idx),
      data = training_df
    )
  })

  cv_splits$splits <- new_splits
  cv_splits
}
