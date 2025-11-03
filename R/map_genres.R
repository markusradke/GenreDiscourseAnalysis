map_metagenres <- function(long, metagenre_mapping) {
  message("Joining metagenre mapping to long format data...")
  long_with_metagenres <- long |>
    dplyr::left_join(
      metagenre_mapping,
      by = "tag_name"
    )
  message("Calculating metagenre prevalences and probabilities...")
  metagenre_preval <- get_metagenre_prevalences(long_with_metagenres)
  all_metagenres <- unique(metagenre_mapping$metagenre)
  message(
    "Formatting metagenre probabilities dataframe and getting hard classes..."
  )
  metagenre_probs_df <- get_metagenre_probs_df(
    metagenre_preval,
    all_metagenres
  )
  metagenre_probs_df
}

get_metagenre_prevalences <- function(long_with_metagenres) {
  total_track_counts <- long_with_metagenres |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::summarize(
      total_counts = sum(.data$tag_count, na.rm = TRUE),
      .groups = "drop"
    )

  metagenre_preval <- long_with_metagenres |>
    dplyr::group_by(.data$track.s.id, .data$metagenre) |>
    dplyr::summarize(
      metagenre_counts = sum(.data$tag_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      total_track_counts,
      by = "track.s.id"
    ) |>
    dplyr::mutate(
      metagenre_prevalence = .data$metagenre_counts / .data$total_counts
    ) |>
    dplyr::arrange(.data$track.s.id, dplyr::desc(.data$metagenre_prevalence))
  return(metagenre_preval)
}

get_metagenre_probs_df <- function(metagenre_prevalences, all_metagenres) {
  expected_cols <- get_expected_column_names(all_metagenres)
  metagenre_probs <- pivot_metagenre_prev_wider(metagenre_prevalences)
  metagenre_probs_full <- add_unmentioned_cols_zero(
    metagenre_probs,
    expected_cols
  )
  get_hard_classes(metagenre_probs_full, metagenre_prevalences)
}

pivot_metagenre_prev_wider <- function(metagenre_prevalences) {
  wide <- metagenre_prevalences |>
    dplyr::distinct(
      .data$track.s.id,
      .data$metagenre,
      .data$metagenre_prevalence
    ) |>
    tidyr::pivot_wider(
      names_from = "metagenre",
      names_prefix = "p_",
      values_from = "metagenre_prevalence",
      values_fill = 0
    )
  colnames(wide) <- gsub(" ", "", colnames(wide))
  wide
}

get_expected_column_names <- function(metagenres) {
  cols <- paste("p_", metagenres, sep = "")
  cols <- gsub(" ", "", cols)
  return(cols)
}

add_unmentioned_cols_zero <- function(df, expected_cols) {
  missing_cols <- expected_cols[!(expected_cols %in% colnames(df))]
  for (col in missing_cols) {
    df[[col]] <- 0
  }
  df
}

get_hard_classes <- function(metagenre_probs_df, metagenre_prevalences) {
  sum_of_prevanlences <- metagenre_prevalences |>
    dplyr::group_by(.data$metagenre) |>
    dplyr::summarize(
      total_prevalence = sum(.data$metagenre_prevalence, na.rm = TRUE),
      .groups = "drop"
    )

  hard_classes <- metagenre_prevalences |>
    dplyr::left_join(
      sum_of_prevanlences,
      by = "metagenre"
    ) |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::arrange(
      dplyr::desc(.data$metagenre_prevalence),
      .data$total_prevalence,
      .data$track.s.id
    ) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select("track.s.id", "metagenre")

  metagenre_probs_with_hard <- metagenre_probs_df |>
    dplyr::left_join(
      hard_classes,
      by = "track.s.id"
    )
  metagenre_probs_with_hard
}
