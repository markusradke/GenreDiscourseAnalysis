#' Plot contingency between Discogs first-level genres and COMGET metagenres
#'
#' Create a tile plot showing relative frequencies of Discogs first-level
#' genres (album.dc.firstgenre) within each COMGET metagenre. Tile fill shows
#' relative frequency and tile labels show absolute counts.
#'
#' @param mb_meta A tibble with columns `track.s.id` and `metagenre`.
#' @param dc_genres A tibble with columns `track.s.id` and
#'   `album.dc.firstgenre`.
#' @return A ggplot2 object (tile plot).
#' @export
plot_dc_contingency <- function(mb_meta, dc_genres, name = "COMGET-G") {
  no_metagenres <- mb_meta$metagenre |> unique() |> length()
  comb <- join_and_filter(mb_meta, dc_genres)
  relfreqs <- compute_relfreqs(comb)
  padded <- relfreqs |>
    tidyr::complete(
      metagenre,
      album.dc.firstgenre,
      fill = list(
        Freq = 0,
        relfreq = 0,
        relfreq_label = 0,
        labelcolor = "black"
      )
    )
  build_plot_dc_contingency(padded, no_metagenres, name)
}

join_and_filter <- function(mb_meta, dc_genres) {
  mb_meta |>
    dplyr::select(track.s.id, metagenre) |>
    dplyr::left_join(
      dplyr::select(dc_genres, track.s.id, album.dc.firstgenre),
      by = "track.s.id"
    ) |>
    dplyr::filter(!is.na(metagenre) & !is.na(album.dc.firstgenre))
}

compute_relfreqs <- function(comb) {
  comb |>
    dplyr::group_by(metagenre, album.dc.firstgenre) |>
    dplyr::tally(name = "Freq") |>
    dplyr::group_by(metagenre) |>
    dplyr::mutate(
      relfreq = Freq / sum(Freq),
      relfreq_label = round(relfreq * 100, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      labelcolor = dplyr::case_when(
        relfreq > 0.5 ~ "white",
        TRUE ~ "black"
      )
    )
}

build_plot_dc_contingency <- function(
  padded,
  no_metagenres,
  name = "COMGET-G"
) {
  p <- ggplot2::ggplot(
    padded,
    ggplot2::aes(
      x = album.dc.firstgenre,
      y = forcats::fct_rev(metagenre),
      fill = relfreq
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(label = relfreq_label, color = labelcolor),
      show.legend = FALSE,
      size = 14 / ggplot2::.pt
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#3e578e",
      labels = scales::percent_format(accuracy = 1),
      name = "",
    ) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = "Discogs",
      y = sprintf("%s%d", name, no_metagenres)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(color = "grey45", size = 16),
      legend.title = ggplot2::element_text(color = "grey45", size = 16),
      axis.text.x = ggplot2::element_text(
        angle = 65,
        hjust = 0,
        vjust = 0,
        size = 16
      ),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(
        color = "grey45",
        size = 18,
        hjust = 0
      ),
      axis.title.y = ggplot2::element_text(
        color = "grey45",
        size = 18,
        hjust = 1
      )
    )
  p
}
