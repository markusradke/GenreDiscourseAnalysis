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

  filtered <- filter_low_frequency_rows(relfreqs, ext_genre_col)
  padded <- filtered |>
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
  build_plot_external_contingency(
    padded,
    relfreqs,
    no_metagenres,
    ext_genre_col,
    name
  )
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
    dplyr::group_by(.data[[ext_genre_col]], metagenre) |>
    dplyr::tally(name = "Freq") |>
    dplyr::group_by(.data[[ext_genre_col]]) |>
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

filter_low_frequency_rows <- function(relfreqs, ext_genre_col) {
  total_n <- sum(relfreqs$Freq)
  row_totals <- relfreqs |>
    dplyr::group_by(.data[[ext_genre_col]]) |>
    dplyr::summarize(total_relfreq = sum(Freq) / total_n, .groups = "drop")

  kept_rows <- row_totals |>
    dplyr::filter(total_relfreq >= 0.005) |>
    dplyr::pull(.data[[ext_genre_col]])

  removed_rows <- row_totals |>
    dplyr::filter(total_relfreq < 0.005) |>
    dplyr::pull(.data[[ext_genre_col]])

  if (length(removed_rows) > 0) {
    message(
      sprintf(
        "Removed %d rows(s) from '%s' with <2%% total frequency: %s",
        length(removed_rows),
        ext_genre_col,
        paste(removed_rows, collapse = ", ")
      )
    )
  }

  relfreqs |>
    dplyr::filter(.data[[ext_genre_col]] %in% kept_rows)
}

translate_deezer_genres <- function(genres) {
  genres <- as.character(genres)
  mapping <- c(
    "Klassik" = "Classical",
    "Filme/Videospiele" = "Stage & Screen",
    "Schlager & Volksmusik" = "Schlager & Volksmusik",
    "Soul & Funk" = "Soul & Funk",
    "Rap/Hip Hop" = "Rap/Hip Hop",
    "Deutsche Musik" = "German Music",
    "Brasilianische Musik" = "Brasilian Music",
    "Asiatische Musik" = "Asian Music",
    "Latin Musik" = "Latin Music",
    "Afrikanische Musik" = "African Music",
    "Türkische Volksmusik" = "Turkish Folk Music",
    "Ranchera" = "Ranchera",
    "Kids" = "Kids",
    "Bolero" = "Bolero",
    "Electro" = "Electro",
    "Disco" = "Disco",
    "R & B" = "R & B",
    "Heavy Metal" = "Heavy Metal",
    "Alternative" = "Alternative",
    "Singer & Songwriter" = "Singer & Songwriter",
    "Country" = "Country",
    "Pop" = "Pop",
    "Rock" = "Rock",
    "Jazz" = "Jazz",
    "Dance" = "Dance"
  )
  translated <- mapping[genres]
  missing <- is.na(translated)
  translated[missing] <- genres[missing]
  unname(translated)
}

get_external_name <- function(ext_genre_col) {
  switch(
    ext_genre_col,
    "track.ab.genrerosamerica" = "Essentia-Predicted Rosamerica Genre",
    "album.dc.firstgenre" = "Discogs First Genre",
    "track.dz.album.firstgenre.name" = "Deezer First Genre",
    ""
  )
}

get_target_colorder <- function(
  ext_genre_col,
  roworder_rosamerica,
  roworder_discogs,
  roworder_deezer
) {
  switch(
    ext_genre_col,
    "track.ab.genrerosamerica" = roworder_rosamerica,
    "album.dc.firstgenre" = roworder_discogs,
    "track.dz.album.firstgenre.name" = roworder_deezer,
    character(0)
  )
}

compute_final_levels <- function(padded, ext_genre_col, target_colorder) {
  present <- padded |>
    dplyr::pull(.data[[ext_genre_col]]) |>
    as.character() |>
    unique()

  ordered_levels <- intersect(target_colorder, present)

  remaining <- padded |>
    dplyr::filter(!(.data[[ext_genre_col]] %in% ordered_levels)) |>
    dplyr::group_by(.data[[ext_genre_col]]) |>
    dplyr::summarize(sum_rel = sum(relfreq), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(sum_rel)) |>
    dplyr::pull(.data[[ext_genre_col]]) |>
    as.character()

  c(ordered_levels, remaining)
}

build_plot_external_contingency <- function(
  padded,
  relfreqs,
  no_metagenres,
  ext_genre_col,
  name = "COMGET-G"
) {
  external_name <- get_external_name(ext_genre_col)

  colorder_full <- c(
    "rock",
    "metal",
    "death metal",
    "heavy metal",
    "power metal",
    "alternative rock",
    "pop rock",
    "hard rock",
    "indie rock",
    "punk",
    "progressive rock",
    "folk",
    "folk rock",
    "classic rock",
    "blues rock",
    "singer-songwriter",
    "blues",
    "country",
    "new wave",
    "soft rock",
    "pop",
    "electronic",
    "house",
    "jazz",
    "schlager",
    "soul",
    "classical",
    "synth-pop",
    "r&b",
    "dance-pop",
    "reggae",
    "hip hop"
  )

  roworder_rosamerica <- c(
    "Rock",
    "Pop",
    "Dance",
    "Rhythm and Blues",
    "Jazz",
    "Classical",
    "Hiphop",
    "Speech"
  )

  roworder_discogs <- c(
    "Rock",
    "Folk, World, & Country",
    "Blues",
    "Pop",
    "Electronic",
    "Funk / Soul",
    "Jazz",
    "Classical",
    "Stage & Screen",
    "Latin",
    "Reggae",
    "Hip Hop",
    "Non-Music",
    "Children's",
    "Brass & Military"
  )

  roworder_deezer <- c(
    "Rock",
    "Heavy Metal",
    "Alternative",
    "Folk",
    "Country",
    "Singer & Songwriter",
    "Pop",
    "Electro",
    "Dance",
    "R&B",
    "Disco",
    "Jazz",
    "Schlager & Volksmusik",
    "Soul & Funk",
    "Klassik",
    "Filme/Videospiele",
    "Rap/Hip Hop",
    "Deutsche Musik",
    "Brasilianische Musik",
    "Asiatische Musik",
    "Latin Musik",
    "Afrikanische Musik",
    "Türkische Volksmusik",
    "Ranchera",
    "Kids",
    "Bolero"
  )

  if (ext_genre_col == "track.dz.album.firstgenre.name") {
    padded <- padded |>
      dplyr::mutate(
        !!ext_genre_col := translate_deezer_genres(
          as.character(.data[[ext_genre_col]])
        )
      )
  }

  target_colorder <- get_target_colorder(
    ext_genre_col,
    roworder_rosamerica,
    roworder_discogs,
    roworder_deezer
  )

  final_levels <- compute_final_levels(padded, ext_genre_col, target_colorder)

  metagenre_freqs <- compute_genre_frequencies(relfreqs, "metagenre")
  external_freqs <- compute_genre_frequencies(relfreqs, ext_genre_col)

  gini_comget <- DescTools::Gini(metagenre_freqs$freq)
  gini_external <- DescTools::Gini(external_freqs$freq)

  external_short_name <- strsplit(external_name, " ")[[1]][1]

  gini_label <- sprintf(
    "Gini<sub>COMGET</sub> = %.3f<br>Gini<sub>%s</sub> = %.3f",
    gini_comget,
    external_short_name,
    gini_external
  )

  padded <- add_frequency_labels(
    padded,
    metagenre_freqs,
    external_freqs,
    ext_genre_col
  )

  padded <- padded |>
    dplyr::mutate(
      !!ext_genre_col := factor(
        as.character(.data[[paste0(ext_genre_col, "_label")]]),
        levels = rev(paste0(
          final_levels,
          " (",
          external_freqs$relfreq_pct[match(final_levels, external_freqs$genre)],
          "%)"
        ))
      ),
      metagenre = factor(
        as.character(.data$metagenre_label),
        levels = paste0(
          colorder_full,
          " (",
          metagenre_freqs$relfreq_pct[match(
            colorder_full,
            metagenre_freqs$genre
          )],
          "%)"
        )
      )
    )

  p <- ggplot2::ggplot(
    padded,
    ggplot2::aes(
      x = .data$metagenre,
      y = .data[[ext_genre_col]],
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
      name = ""
    ) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title = gini_label,
      x = sprintf("%s%d", name, no_metagenres),
      y = external_name
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 16,
        face = "bold",
        color = "black",
        hjust = 0,
        vjust = -0.5,
        lineheight = 1.5,
        margin = ggplot2::margin(b = -72)
      ),
      plot.title.position = "plot",
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

compute_genre_frequencies <- function(padded, genre_col) {
  padded |>
    dplyr::group_by(.data[[genre_col]]) |>
    dplyr::summarize(freq = sum(Freq), .groups = "drop") |>
    dplyr::mutate(
      relfreq = freq / sum(freq),
      relfreq_pct = round(relfreq * 100, 0)
    ) |>
    dplyr::rename(genre = .data[[genre_col]])
}

add_frequency_labels <- function(
  padded,
  metagenre_freqs,
  external_freqs,
  ext_genre_col
) {
  padded |>
    dplyr::left_join(
      metagenre_freqs |>
        dplyr::select(genre, relfreq_pct) |>
        dplyr::rename(metagenre_pct = relfreq_pct),
      by = c("metagenre" = "genre")
    ) |>
    dplyr::left_join(
      external_freqs |>
        dplyr::select(genre, relfreq_pct) |>
        dplyr::rename(external_pct = relfreq_pct),
      by = setNames("genre", ext_genre_col)
    ) |>
    dplyr::mutate(
      metagenre_label = paste0(metagenre, " (", metagenre_pct, "%)"),
      !!paste0(ext_genre_col, "_label") := paste0(
        .data[[ext_genre_col]],
        " (",
        external_pct,
        "%)"
      )
    )
}
