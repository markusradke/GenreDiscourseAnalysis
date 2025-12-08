rm(list = ls())
gc()
devtools::load_all()

min_votes <- 1
min_prop <- 0

tune_mb <- readRDS("models/metagenres/tune_mb_metagenres.rds")
tune_s <- readRDS("models/metagenres/tune_s_metagenres.rds")

# Map metagenres for solution ranges ----
message("Mapping metagenres for solution ranges...")
map_genres_solution_range <- function(
  long,
  tune,
  n_range,
  min_votes = 1,
  min_prop = 0.
) {
  suggested_min_n <- tune$ginis |>
    dplyr::filter(n_metagenres >= n_range[1], n_metagenres <= n_range[2]) |>
    dplyr::arrange(weighted_gini, dplyr::desc(min_n)) |>
    dplyr::slice(1) |>
    dplyr::pull(min_n)
  suggested <- tune$solutions[[as.character(suggested_min_n)]]
  map_metagenres(long, suggested$mapping) |>
    dplyr::filter(n_max >= min_votes) |>
    dplyr::filter(p_max >= min_prop)
}

mb_25_30 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  c(25, 30),
  min_votes,
  min_prop
)
save_feather_with_lists(
  mb_25_30,
  "models/metagenres/mb_metagenres_25_30.feather"
)

s_25_30 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  c(25, 30),
  min_votes,
  min_prop
)
save_feather_with_lists(
  s_25_30,
  "models/metagenres/s_metagenres_25_30.feather"
)

mb_10_15 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_mb_long.feather"),
  tune_mb,
  c(10, 15),
  min_votes,
  min_prop
)
save_feather_with_lists(
  mb_10_15,
  "models/metagenres/mb_metagenres_10_15.feather"
)

s_10_15 <- map_genres_solution_range(
  read_feather_with_lists("data/filtered_s_long.feather"),
  tune_s,
  c(10, 15),
  min_votes,
  min_prop
)
save_feather_with_lists(
  s_10_15,
  "models/metagenres/s_metagenres_10_15.feather"
)

generate_report("04_metagenres_mapping")
