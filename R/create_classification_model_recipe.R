create_pre_recipe <- function(
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
    themis::step_nearmiss(
      metagenre,
      under_ratio = if (tune_downsample) {
        tune::tune()
      } else {
        under_ratio_fix
      },
      neighbors = 5,
      seed = seed,
      skip = TRUE
    ) |>
    themis::step_bsmote(
      metagenre,
      over_ratio = if (tune_upsample) {
        tune::tune()
      } else {
        over_ratio_fix
      },
      neighbors = 5,
      seed = seed,
      skip = TRUE
    ) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(
      recipes::all_numeric_predictors()
    )
}
