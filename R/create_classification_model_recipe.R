create_pre_recipe <- function(
  train_df,
  vars_to_remove,
  tune_sampling = TRUE,
  target_ratio_fix = 5,
  seed = 42
) {
  vars_to_remove <- union(vars_to_remove, c("track.s.id", "artist.s.id"))
  recipe_schema <- train_df[1, , drop = FALSE]

  recipes::recipe(metagenre ~ ., data = recipe_schema) |>
    recipes::step_rm(dplyr::all_of(vars_to_remove)) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    step_adaptive_sampling(
      metagenre,
      target_ratio = if (tune_sampling) tune::tune() else target_ratio_fix,
      seed = seed,
      skip = TRUE
    ) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}
