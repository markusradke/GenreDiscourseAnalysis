classification_prep_features <- readRDS(
  "data-raw/classification_prep_features.rds"
)
artist_origin_lookup <- readRDS(
  "data-raw/artist_origin_lookup.rds"
)

usethis::use_data(
  classification_prep_features,
  artist_origin_lookup,
  overwrite = TRUE,
  internal = TRUE
)
