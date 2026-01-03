rm(list = ls())
gc()
devtools::load_all()

min_votes <- 1
min_prop <- 0

tune_mb <- readRDS("models/metagenres/tune_mb_metagenres.rds")
tune_s <- readRDS("models/metagenres/tune_s_metagenres.rds")

message("Mapping metagenres for solution ranges...")

mb_low <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  5,
  min_votes,
  min_prop
)

mb_medium <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  12,
  min_votes,
  min_prop
)


mb_high <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  25,
  min_votes,
  min_prop
)

mb_very_high <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  32,
  min_votes,
  min_prop
)

save_feather_with_lists(
  mb_low,
  "models/metagenres/mb_metagenres_low.feather"
)
save_feather_with_lists(
  mb_medium,
  "models/metagenres/mb_metagenres_medium.feather"
)
save_feather_with_lists(
  mb_high,
  "models/metagenres/mb_metagenres_high.feather"
)
save_feather_with_lists(
  mb_very_high,
  "models/metagenres/mb_metagenres_very_high.feather"
)


s_low <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  7,
  min_votes,
  min_prop
)

s_medium <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  14,
  min_votes,
  min_prop
)


s_high <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  24,
  min_votes,
  min_prop
)

s_very_high <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  31,
  min_votes,
  min_prop
)

save_feather_with_lists(
  s_low,
  "models/metagenres/s_metagenres_low.feather"
)
save_feather_with_lists(
  s_medium,
  "models/metagenres/s_metagenres_medium.feather"
)
save_feather_with_lists(
  s_high,
  "models/metagenres/s_metagenres_high.feather"
)
save_feather_with_lists(
  s_very_high,
  "models/metagenres/s_metagenres_very_high.feather"
)


generate_report("04_mapping")
