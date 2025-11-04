#' Map tracks to metagenres with probabilities and hard class
#'
#' This function is pure and returns a data.frame with one row per track,
#' probability columns p_<metagenre> and a chosen metagenre column.
map_metagenres <- function(long, metagenre_mapping) {
  long_with_meta <- join_metagenre_mapping(long, metagenre_mapping)
  preval <- get_metagenre_prevalences(long_with_meta)
  all_meta <- unique(metagenre_mapping$metagenre)
  get_metagenre_probs_df(preval, all_meta)
}

join_metagenre_mapping <- function(long, mapping) {
  dplyr::left_join(long, mapping, by = "tag_name")
}

compute_total_counts <- function(df) {
  df |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::summarize(
      total_counts = sum(.data$tag_count, na.rm = TRUE),
      .groups = "drop"
    )
}

compute_meta_counts <- function(df) {
  df |>
    dplyr::group_by(.data$track.s.id, .data$metagenre) |>
    dplyr::summarize(
      metagenre_counts = sum(.data$tag_count, na.rm = TRUE),
      .groups = "drop"
    )
}

get_metagenre_prevalences <- function(long_with_metagenres) {
  totals <- compute_total_counts(long_with_metagenres)
  meta_counts <- compute_meta_counts(long_with_metagenres)
  meta_counts |>
    dplyr::left_join(totals, by = "track.s.id") |>
    dplyr::mutate(
      metagenre_prevalence = metagenre_counts / total_counts
    ) |>
    dplyr::arrange(.data$track.s.id, dplyr::desc(.data$metagenre_prevalence))
}

get_metagenre_probs_df <- function(metagenre_prevalences, all_metagenres) {
  expected_cols <- get_expected_column_names(all_metagenres)
  probs_wide <- pivot_metagenre_prev_wider(metagenre_prevalences)
  probs_full <- add_unmentioned_cols_zero(probs_wide, expected_cols)
  probs_max <- add_max_prob_cols(probs_full)
  get_hard_classes(probs_max, metagenre_prevalences)
}

sanitize_name <- function(x) {
  gsub("[ &-]", "", x)
}

pivot_metagenre_prev_wider <- function(metagenre_prevalences) {
  wide_p <- metagenre_prevalences |>
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
  wide_n <- metagenre_prevalences |>
    dplyr::distinct(
      .data$track.s.id,
      .data$metagenre,
      .data$metagenre_counts
    ) |>
    tidyr::pivot_wider(
      names_from = "metagenre",
      names_prefix = "n_",
      values_from = "metagenre_counts",
      values_fill = 0
    )
  wide <- dplyr::left_join(wide_p, wide_n, by = "track.s.id")
  colnames(wide) <- sanitize_name(colnames(wide))
  wide
}

get_expected_column_names <- function(metagenres) {
  cols <- paste("p_", metagenres, sep = "")
  cols <- c(cols, (paste("n_", metagenres, sep = "")))
  sanitize_name(cols)
}

add_unmentioned_cols_zero <- function(df, expected_cols) {
  missing_cols <- expected_cols[!(expected_cols %in% colnames(df))]
  for (col in missing_cols) {
    df[[col]] <- 0
  }
  df
}

compute_sum_prevalence <- function(metagenre_prevalences) {
  metagenre_prevalences |>
    dplyr::group_by(.data$metagenre) |>
    dplyr::summarize(
      total_prevalence = sum(.data$metagenre_prevalence, na.rm = TRUE),
      .groups = "drop"
    )
}

get_hard_classes <- function(metagenre_probs_df, metagenre_prevalences) {
  sum_prev <- compute_sum_prevalence(metagenre_prevalences)
  hard <- metagenre_prevalences |>
    dplyr::left_join(sum_prev, by = "metagenre") |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::arrange(
      dplyr::desc(.data$metagenre_prevalence),
      .data$total_prevalence,
      .data$track.s.id
    ) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select("track.s.id", "metagenre")
  dplyr::left_join(metagenre_probs_df, hard, by = "track.s.id")
}

add_max_prob_cols <- function(df) {
  p_cols <- colnames(df)[grep("^p_", colnames(df))]
  df$p_max <- apply(
    df[, p_cols],
    1,
    max,
    na.rm = TRUE
  )
  n_cols <- colnames(df)[grep("^n_", colnames(df))]
  df$n_max <- apply(
    df[, n_cols],
    1,
    max,
    na.rm = TRUE
  )
  df
}
