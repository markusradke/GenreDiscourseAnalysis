create_base_recipe <- function(
  train_df,
  vars_to_remove,
  tune_downsample = TRUE,
  tune_upsample = TRUE,
  under_ratio_fix = 2,
  over_ratio_fix = 0.5,
  seed = 42
) {
  vars_to_remove <- union(vars_to_remove, c("track.s.id", "artist.s.id"))
  recipe_schema <- train_df[1, , drop = FALSE] # only gets caluclated during training, save memory
  recipes::recipe(metagenre ~ ., data = recipe_schema) |>
    recipes::step_rm(dplyr::all_of(vars_to_remove)) |>
    recipes::step_impute_median(
      recipes::all_numeric_predictors()
    ) |>
    recipes::step_impute_mode(
      recipes::all_nominal_predictors()
    ) |>
    themis::step_downsample(
      metagenre,
      under_ratio = if (tune_downsample) {
        tune::tune()
      } else {
        under_ratio_fix
      },
      seed = seed,
      skip = TRUE
    ) |>
    themis::step_smotenc(
      metagenre,
      over_ratio = if (tune_upsample) {
        tune::tune()
      } else {
        over_ratio_fix
      },
      seed = seed,
      skip = TRUE
    ) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(
      recipes::all_numeric_predictors()
    )
}

create_glmnet_recipe <- function(
  train_df,
  vars_to_remove,
  tune_downsample = TRUE,
  tune_upsample = TRUE,
  under_ratio_fix = 2,
  over_ratio_fix = 0.5,
  seed = 42
) {
  create_base_recipe(
    train_df,
    vars_to_remove,
    tune_downsample = tune_downsample,
    tune_upsample = tune_upsample,
    under_ratio_fix = under_ratio_fix,
    over_ratio_fix = over_ratio_fix,
    seed = seed
  ) |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)
}

create_rf_recipe <- function(
  train_df,
  vars_to_remove,
  tune_downsample = TRUE,
  tune_upsample = TRUE,
  under_ratio_fix = 2,
  over_ratio_fix = 0.5,
  seed = 42
) {
  create_base_recipe(
    train_df,
    vars_to_remove,
    tune_downsample = tune_downsample,
    tune_upsample = tune_upsample,
    under_ratio_fix = under_ratio_fix,
    over_ratio_fix = over_ratio_fix,
    seed = seed
  )
}
