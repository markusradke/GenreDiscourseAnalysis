plot_tuning_results <- function(
  ginis,
  xlimits = c(0, max(ginis$n_metagenres)),
  best_candidate = 20,
  gini_range = c(0, 1)
) {
  if (nrow(distinct(ginis, n_metagenres)) < 2) {
    message('Found only a single solution. Thus, no plot will be generated.')
    return(NULL)
  }
  n_metagenre_candidates <- get_local_minima_candidates(ginis) |>
    dplyr::pull(n_metagenres)
  candidates <- ginis |>
    dplyr::filter(n_metagenres %in% n_metagenre_candidates) |>
    dplyr::arrange(-n_metagenres, -min_n) |>
    dplyr::distinct(n_metagenres, .keep_all = T) |>
    dplyr::mutate(
      label = sprintf('%d genres\nmin n: %d', n_metagenres, min_n),
      color = ifelse(n_metagenres == best_candidate, F, T)
    )
  gini_trajectory <- ginis |>
    dplyr::distinct(n_metagenres, weighted_gini, .keep_all = T)

  plot <- ggplot2::ggplot(
    gini_trajectory,
    ggplot2::aes(x = n_metagenres, y = weighted_gini)
  ) +
    ggplot2::geom_line(color = 'grey30', linewidth = 1) +
    ggplot2::geom_point(
      data = candidates,
      mapping = aes(x = n_metagenres, y = weighted_gini),
      color = '#c40d20',
      size = 3
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(gini_range) +
    ggplot2::ylab('Weighted Gini') +
    ggplot2::scale_x_continuous(
      breaks = pretty(seq(xlimits[1], xlimits[2]), n = 10),
      limits = xlimits,
      name = '# of metagenres'
    ) +
    ggplot2::scale_size(range = c(1, 4), name = 'Minimum n') +
    ggplot2::theme(
      axis.title = ggplot2::element_text(color = 'grey30'),
      axis.text.x = ggplot2::element_text(color = 'grey30', size = 12),
      axis.text.y = ggplot2::element_text(color = c('grey30'), size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(color = 'grey30', size = 12)
    ) +
    ggplot2::labs(
      title = 'Weighted within-level Gini for different minimum n per metagenre',
      subtitle = sprintf(
        'caluclated for min n from %d to %d in steps of %d\n# of resulting metagenres ranged from %d to %d ',
        min(ginis$min_n),
        max(ginis$min_n),
        unique(diff(ginis$min_n)),
        min(ginis$n_metagenres),
        max(ginis$n_metagenres)
      )
    ) +
    ggrepel::geom_label_repel(
      data = candidates,
      mapping = aes(
        x = n_metagenres,
        y = weighted_gini,
        label = label,
        color = color
      ),
      show.legend = F,
      box.padding = 0.5
    ) +
    ggplot2::scale_color_manual(values = c('black', 'grey30')) +
    ggplot2::geom_point(
      data = data.frame(x = 0, y = 1),
      aes(x = x, y = y),
      color = '#c40d20',
      size = 3
    ) +
    ggplot2::annotate(
      'text',
      x = 0,
      y = 1,
      label = ' = local Gini minima',
      size = 10,
      size.unit = 'pt',
      color = '#c40d20',
      hjust = -0.05
    )
  plot
}

plot_metagenre_distributions <- function(
  metagenre_mapping,
  initial_genres,
  ncol
) {
  mapping_statistics <- get_mapping_statistics(
    metagenre_mapping,
    initial_genres
  )
  p <- ggplot2::ggplot(
    mapping_statistics,
    ggplot2::aes(y = fct_rev(fct_inorder(initial_genre)), x = n)
  ) +
    ggplot2::geom_col(aes(fill = is_metagenre), show.legend = F) +
    ggplot2::scale_fill_manual(values = c('grey85', 'grey70')) +
    ggplot2::facet_wrap(vars(label_meta), scales = 'free_y', ncol = ncol) +
    ggplot2::labs(x = '# of tracks in data set', y = '') +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      position = 'top',
      expand = c(0.025, 0.05),
      labels = scales::comma_format(big.mark = ",", decimal.mark = ".")
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        color = 'grey35',
        size = 12,
        hjust = 0
      ),
      axis.text.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = 'grey60',
        linewidth = 0.1,
        fill = NA
      ),
      panel.spacing.x = ggplot2::unit(0, "line")
    ) +
    ggtext::geom_richtext(
      ggplot2::aes(x = Inf, y = 0, label = label_meta),
      hjust = 1,
      vjust = 0,
      color = 'grey35',
      size = 4,
      nudge_x = -5000,
      label.colour = 'white',
      inherit.aes = FALSE
    )

  p <- p +
    ggplot2::geom_text(
      data = mapping_statistics |> dplyr::filter(is_metagenre),
      ggplot2::aes(x = 0, y = initial_genre, label = initial_genre),
      hjust = 0,
      vjust = 0.5,
      color = '#c40d20',
      fontface = 'bold',
      size = 5,
      inherit.aes = FALSE
    )

  # Add regular y-axis labels for non-metagenres
  p <- p +
    ggplot2::geom_text(
      data = mapping_statistics |> dplyr::filter(!is_metagenre),
      ggplot2::aes(x = 0, y = initial_genre, label = initial_genre),
      hjust = 0,
      vjust = 0.5,
      color = 'grey35',
      size = 4,
      inherit.aes = FALSE
    )
  p
}

get_mapping_statistics <- function(metagenre_mapping, initial_genres) {
  n_subgenres <- metagenre_mapping |>
    dplyr::distinct(initial_genre, metagenre) |>
    dplyr::count(metagenre) |>
    dplyr::mutate(n_subgenres = n - 1) |>
    dplyr::select(metagenre, n_subgenres)

  metagenre_labels <- dplyr::count(metagenre_mapping, metagenre) |>
    dplyr::rename(n_metagenre = n) |>
    dplyr::left_join(n_subgenres) |>
    dplyr::mutate(
      rel_freq_metagenre = n_metagenre / sum(n_metagenre),
      label_meta = sprintf(
        "<span style='color:#000000; font-size:14pt; font-weight:bold;'>%.2f%%</span> of tracks<br><span style='color:#000000; font-size:14pt; font-weight:bold;'>%d</span> subgenres",
        rel_freq_metagenre * 100,
        n_subgenres
      )
    )

  initial_genres |>
    dplyr::inner_join(metagenre_mapping) |>
    dplyr::group_by(metagenre) |>
    dplyr::mutate(initial_genre = fct_lump_n(initial_genre, n = 10)) |>
    dplyr::ungroup() |>
    dplyr::count(initial_genre, metagenre) |>
    dplyr::arrange(metagenre, -n) |>
    dplyr::inner_join(metagenre_labels) |>
    dplyr::mutate(is_metagenre = ifelse(initial_genre == metagenre, T, F))
}
