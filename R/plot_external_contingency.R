#' Plot contingency between Discogs first-level genres and COMGET metagenres
#'
#' Create a tile plot showing relative frequencies of Discogs first-level
#' genres (album.dc.firstgenre) within each COMGET metagenre. Tile fill shows
#' relative frequency and tile labels show absolute counts.
#'
#' @param mb_meta A tibble with columns `track.s.id` and `metagenre`.
#' @param poptrag A tibble with columns `track.s.id` and external genre col
#' @param external_genre_col either  `album.dc.firstgenre`, `track.dz.album.firstgenre.name`
#'   or `track.ab.genrerosamerica`.
#' @return A ggplot2 object (tile plot).
#' @export
plot_external_contingency <- function(
  mb_meta,
  poptrag,
  ext_genre_col = "album.dc.firstgenre",
  name = "COMGET-G"
) {
  stopifnot(
    ext_genre_col %in%
      c(
        "album.dc.firstgenre",
        "track.dz.album.firstgenre.name",
        "track.ab.genrerosamerica"
      )
  )
  no_metagenres <- mb_meta$metagenre |> unique() |> length()
  comb <- join_and_filter(mb_meta, poptrag, ext_genre_col)
  relfreqs <- compute_relfreqs(comb, ext_genre_col)
  padded <- relfreqs |>
    tidyr::complete(
      metagenre,
      .data[[ext_genre_col]],
      fill = list(
        Freq = 0,
        relfreq = 0,
        relfreq_label = 0,
        labelcolor = "black"
      )
    )
  build_plot_external_contingency(padded, no_metagenres, ext_genre_col, name)
}

join_and_filter <- function(mb_meta, poptrag, ext_genre_col) {
  mb_meta |>
    dplyr::select(track.s.id, metagenre) |>
    dplyr::left_join(
      dplyr::select(poptrag, track.s.id, dplyr::all_of(ext_genre_col)),
      by = "track.s.id"
    ) |>
    dplyr::filter(!is.na(metagenre) & !is.na(.data[[ext_genre_col]]))
}

compute_relfreqs <- function(comb, ext_genre_col = "album.dc.firstgenre") {
  comb |>
    dplyr::group_by(metagenre, .data[[ext_genre_col]]) |>
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

build_plot_external_contingency <- function(
  padded,
  no_metagenres,
  ext_genre_col,
  name = "COMGET-G"
) {
  external_name <- dplyr::case_when(
    ext_genre_col == "album.dc.firstgenre" ~ "Discogs First Genre",
    ext_genre_col == "track.dz.album.firstgenre.name" ~ "Deezer First Genre",
    ext_genre_col ==
      "track.ab.genrerosamerica" ~ "Essentia-Predicted Rosamerica Genre"
  )

  p <- ggplot2::ggplot(
    padded,
    ggplot2::aes(
      x = .data[[ext_genre_col]],
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
      x = external_name,
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
