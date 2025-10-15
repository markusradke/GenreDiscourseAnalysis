#' Fold genre tree and caluclate weighted ginis
#'
#' Folds a given genre tree from the end of branches, so that all remaining genres have at least a minimum number of observations. Caluclates a weighted Gini for each solution (Gini is caluclated for each level in the hierarchy accross all Genres in the level with their prevalences and ist then averaged, weighted by the number of songs in each level).
#'
#' @param initial_genres data.frame with initial genres (and id for later mapping). When searching for subgenres, a column "metagenre" must also be given.
#' @param graph hierarchical genre tree
#' @param optimal_solution_range_n_metagenres optimal range of number of metagenres for solution. Used for choosing the suggested solution (chooses local optima within range with highest number of metagenres). If no local optima for the Gini are found within solution range, the local optiima below the solution range with the highest number of metagenres is chosen. If no local optima below solution range, Take lowest gini within solution range. If no solution within solution range take global local minimum of Gini.
#' @param min_n_grid_min Training grid minimum of minimum n.
#' @param min_n_grid_max Training grid minimum of minimum n. Optional, defaults to highest possible whith at least one split in the resulting tree.
#' @param min_n_grid_step Granularity of the grid.
#' @param root Root genre of the tree.

#' @returns List object with all solutions (min n as string is name of solution), a dataframe with gini metrics, a suggested solution in the optimal range, and a weighted Gini plot.
#'
tune_tree_folding <- function(
  initial_genres,
  graph,
  optimal_solution_range_n_metagenres,
  min_n_grid_min = 100,
  min_n_grid_max = integer(),
  min_n_grid_step = 20,
  root = 'POPULAR MUSIC'
) {
  graph_root <- get_subgraph(graph, root)
  if (!'metagenre' %in% colnames(initial_genres)) {
    initial_genres <- dplyr::mutate(initial_genres, metagenre = root)
  }
  initial_genres <- initial_genres |>
    dplyr::filter(metagenre == root)

  search_grid_min_n <- get_search_grid(
    min_n_grid_min,
    min_n_grid_max,
    min_n_grid_step,
    initial_genres
  )

  tuning <- tune_by_folding_genre_tree_bottom_to_top(
    initial_genres,
    graph_root,
    search_grid_min_n,
    root
  )
  tuning$root <- root
  tuning <- handle_non_gini_solutions(tuning)
  tuning$candidates <- get_local_minima_candidates(tuning$ginis)
  if (nrow(tuning$ginis) > 0) {
    tuning$suggested_solution <- get_suggested_solution(
      tuning,
      optimal_solution_range_n_metagenres
    )
  } else {
    tuning$suggested_solution <- tuning$solutions[[1]]
  }
  n_metagenres_suggested <- tuning$suggested_solution$n_songs |> nrow()
  tuning$plot <- plot_tuning_results(
    tuning$ginis,
    xlimits = c(0, min(max(tuning$ginis$n_metagenres), 50)),
    best_candidate = n_metagenres_suggested,
  )
  tuning
}

handle_non_gini_solutions <- function(tuning) {
  is_only_nan_gini <- tuning$ginis |>
    dplyr::filter(!is.na(weighted_gini)) |>
    nrow() ==
    0
  if (is_only_nan_gini) {
    tuning <- get_non_gini_solutions(tuning)
  } else {
    tuning <- remove_non_gini_solutions(tuning)
  }
}

get_non_gini_solutions <- function(tuning) {
  tuning$ginis <- head(tuning$ginis, 0)
  tuning$solutions <- tuning$solutions
  tuning
}

remove_non_gini_solutions <- function(tuning) {
  # nan Gini is always the last solution calculated
  tuning$solutions <- head(tuning$solutions, -1)
  tuning$ginis <- head(tuning$ginis, -1)
  tuning
}

get_search_grid <- function(
  min_n_grid_min,
  min_n_grid_max,
  min_n_grid_step,
  initial_genres
) {
  if (length(min_n_grid_max) == 0) {
    upper_bound <- initial_genres |>
      dplyr::filter(!is.na(initial_genre)) |>
      dplyr::count(initial_genre, sort = T) |>
      dplyr::first() |>
      dplyr::pull(n)
    min_n_grid_max <- upper_bound
  }
  seq(min_n_grid_min, min_n_grid_max, min_n_grid_step)
}

tune_by_folding_genre_tree_bottom_to_top <- function(
  initial_genres,
  graph_connected,
  min_n_grid,
  root = 'rock'
) {
  res <- list()
  ginis <- c()
  if ('metagenre' %in% colnames(initial_genres)) {
    initial_genres <- dplyr::rename(initial_genres, 'root' = 'metagenre')
  }
  get_distances_to_root <- get_distances_to_root(graph_connected)
  for (i in seq_along(min_n_grid)) {
    message(sprintf('Folding tree for minimum n = %d...', min_n_grid[i]))
    temp <- fold_genre_tree_bottom_to_top(
      initial_genres,
      graph_connected,
      min_n_grid[i],
      root
    )
    ginis <- rbind(
      ginis,
      data.frame(
        n_metagenres = nrow(temp$n_songs),
        min_n = min_n_grid[i],
        weighted_gini = get_gini_coefficient(
          temp$mapping,
          get_distances_to_root
        )
      )
    )
    if (i > 1) {
      res[[toString(min_n_grid[i])]] <- temp
      is_new_n_metagenres <- ginis$n_metagenres[i] != ginis$n_metagenres[i - 1]
      if (!is_new_n_metagenres) {
        res[[toString(min_n_grid[i - 1])]] <- NULL
      }
    }
    if (is.nan(tail(ginis, 1)$weighted_gini)) {
      message('Stopping search: Gini computation was not possible anymore')
      break
    }
  }
  res <- list(solutions = res, ginis = ginis)
  res
}

fold_genre_tree_bottom_to_top <- function(
  initial_genres,
  graph_connected,
  min_n,
  root = 'rock'
) {
  hierarchy <- get_distances_to_root(graph_connected) |>
    dplyr::mutate(checked = F)
  current_mapping <- initial_genres |> dplyr::mutate(genre = initial_genre)
  current_graph <- graph_connected
  n_songs <- dplyr::count(current_mapping, genre)
  i <- 0
  n_genres <- nrow(hierarchy)
  repeat {
    current_genre <- get_current_genre(hierarchy, n_songs)
    if (
      dplyr::filter(hierarchy, tag_name == current_genre) |>
        dplyr::pull(hierarchy_level) ==
        0
    ) {
      break
    }
    n_current_genre <- get_current_genre_n(current_genre, n_songs)
    if (n_current_genre > min_n) {
      is_remaining_parent_larger_min_n <- is_parent_and_remaining_larger_min_n(
        current_genre,
        current_graph,
        n_songs,
        min_n
      )
      if (is_remaining_parent_larger_min_n) {
        current_graph <- fold_new_metagenre_from_graph(
          current_genre,
          current_graph
        )
        hierarchy <- set_checked_flag_current_genre(current_genre, hierarchy)
      } else {
        temp <- integrate_genre_and_siblings(
          current_genre,
          current_mapping,
          current_graph,
          hierarchy
        )
        current_mapping <- temp$current_mapping
        n_songs <- temp$n_songs
        hierarchy <- temp$hierarchy
      }
    } else {
      temp <- integrate_genre_and_siblings(
        current_genre,
        current_mapping,
        current_graph,
        hierarchy
      )
      current_mapping <- temp$current_mapping
      n_songs <- temp$n_songs
      hierarchy <- temp$hierarchy
    }
    i <- i + 1
  }
  metagenre_graph <- get_metagenre_graph(current_mapping, graph_connected)
  list(
    mapping = current_mapping |> dplyr::rename(metagenre = genre),
    segmented_graph = current_graph,
    metagenre_graph = metagenre_graph,
    min_n = min_n,
    n_songs = n_songs
  )
}

get_metagenre_graph <- function(mapping, graph) {
  metagenres <- unique(mapping$genre)
  igraph::induced_subgraph(graph, V(graph)[name %in% metagenres])
}


integrate_genre_and_siblings <- function(genre, mapping, graph, hierarchy) {
  supergenre <- names(igraph::neighbors(graph, genre, mode = "out"))
  all_subgenres <- names(igraph::neighbors(graph, supergenre, mode = 'in'))
  new_mapping <- mapping |>
    dplyr::mutate(genre = ifelse(genre %in% all_subgenres, supergenre, genre))
  n_songs <- dplyr::count(new_mapping, genre)
  hierarchy <- hierarchy |>
    dplyr::mutate(checked = ifelse(tag_name %in% all_subgenres, T, checked))
  list(current_mapping = new_mapping, n_songs = n_songs, hierarchy = hierarchy)
}

get_current_genre <- function(hierarchy, n_songs) {
  hierarchy |>
    dplyr::left_join(n_songs, by = dplyr::join_by('tag_name' == 'genre')) |>
    dplyr::mutate(n = ifelse(is.na(n), 0, n)) |>
    dplyr::filter(!checked) |>
    dplyr::arrange(-hierarchy_level, -n) |>
    dplyr::first() |>
    dplyr::pull(tag_name)
}

is_parent_and_remaining_larger_min_n <- function(genre, graph, n_songs, min_n) {
  supergenre <- names(igraph::neighbors(graph, genre, mode = "out"))
  if (length(supergenre) == 0) {
    return()
  }
  other_subgenres <- names(igraph::neighbors(graph, supergenre, mode = 'in'))
  other_subgenres <- other_subgenres[other_subgenres != genre]
  remaining <- c(supergenre, other_subgenres)
  dplyr::filter(n_songs, genre %in% remaining) |>
    dplyr::pull(n) |>
    sum() >
    min_n
}

get_current_genre_n <- function(current_genre, n_songs) {
  current_n <- n_songs |>
    dplyr::filter(genre == current_genre) |>
    dplyr::pull(n)
  if (length(current_n) == 0) {
    current_n <- 0
  }
  current_n
}

fold_new_metagenre_from_graph <- function(current_genre, current_graph) {
  outgoing_edge <- igraph::E(current_graph)[.from(current_genre)]
  igraph::delete_edges(current_graph, outgoing_edge)
}

set_checked_flag_current_genre <- function(current_genre, hierarchy) {
  hierarchy |>
    dplyr::mutate(checked = ifelse(tag_name == current_genre, T, checked))
}

get_gini_coefficient <- function(metagenres, get_distances_to_root) {
  relfreqs <- metagenres |>
    dplyr::count(metagenre) |>
    dplyr::mutate(relfreq = n / nrow(metagenres)) |>
    dplyr::left_join(
      get_distances_to_root,
      by = dplyr::join_by('metagenre' == 'tag_name')
    ) |>
    dplyr::filter(!is.na(hierarchy_level))
  ginis <- relfreqs |>
    dplyr::group_by(hierarchy_level) |>
    dplyr::summarize(gini = DescTools::Gini(relfreq))
  counts <- relfreqs |>
    dplyr::count(hierarchy_level) |>
    dplyr::filter(n > 1) # Gini is only defined for more than 1 entries
  weighted_gini <- ginis |>
    dplyr::inner_join(
      counts,
      by = dplyr::join_by('hierarchy_level' == 'hierarchy_level')
    ) |>
    dplyr::mutate(addend = gini * n)
  weighted_gini <- sum(weighted_gini$addend) / sum(weighted_gini$n)
  weighted_gini
}

get_local_minima_candidates <- function(ginis) {
  candidate_pool <- ginis |>
    dplyr::arrange(n_metagenres, -min_n) |>
    dplyr::distinct(weighted_gini, n_metagenres, .keep_all = TRUE)
  candidate_pool <-
    dplyr::bind_rows(
      data.frame(weighted_gini = 1, n_metagenres = 1),
      candidate_pool
    ) |>
    dplyr::bind_rows(data.frame(weighted_gini = 0, n_metagenres = Inf))
  from_left <- which(diff(candidate_pool$weighted_gini) < 0) + 1
  from_right <- nrow(candidate_pool) -
    which(diff(rev(candidate_pool$weighted_gini)) < 0)
  candidates <- intersect(from_left, from_right)
  candidate_pool[candidates, ]
}


get_suggested_solution <- function(tuning, solution_range) {
  suggestions <- tuning$candidates |>
    dplyr::filter(
      n_metagenres >= solution_range[1] &
        n_metagenres <= solution_range[2]
    ) |>
    dplyr::arrange(-n_metagenres)
  if (nrow(suggestions) == 0) {
    # if nothing in range take local min with highest # of metagenres from below
    suggestions <- tuning$candidates |>
      dplyr::filter(n_metagenres <= solution_range[2]) |>
      dplyr::arrange(-n_metagenres)
  }
  if (nrow(suggestions) == 0) {
    # if nothing below take  local main take local min with lowest global # of metagenres
    suggestions <- tuning$candidates |>
      dplyr::arrange(n_metagenres)
  }
  if (nrow(suggestions) == 0) {
    # if no local min at all take global min within solution range
    suggestions <- tuning$ginis |>
      dplyr::filter(
        n_metagenres >= solution_range[1] &
          n_metagenres <= solution_range[2]
      ) |>
      dplyr::arrange(weighted_gini)
  }
  if (nrow(suggestions) == 0) {
    # if no solution in solution range take global min
    suggestions <- tuning$ginis |>
      dplyr::arrange(weighted_gini)
  }
  suggestion_n_metagenres <- suggestions[1, ]$n_metagenres
  get_n_metagenre_solution_from_tuning_results(tuning, suggestion_n_metagenres)
}

get_n_metagenre_solution_from_tuning_results <- function(tuning, n_genres) {
  min_n_solution <- tuning$ginis |>
    dplyr::filter(n_metagenres == n_genres) |>
    dplyr::arrange(-min_n) |>
    dplyr::first() |>
    dplyr::pull(min_n) |>
    toString()
  tuning$solutions[[min_n_solution]]
}

get_ideal_tree_from_tuning <- function(ideal_tree_tuning) {
  ideal_tree_mapping <- lapply(
    ideal_tree_tuning,
    function(metagenre) {
      metagenre$suggested_solution$mapping
    }
  ) |>
    do.call(bind_rows, .)
  ideal_tree_submetagenres <- unique(ideal_tree_mapping$metagenre)
  delete_ids <- igraph::V(graph_connected)[
    !names(V(graph_connected)) %in% ideal_tree_submetagenres
  ]
  ideal_tree_graph <- igraph::delete_vertices(graph_connected, delete_ids)
  res <- list()
  res$mapping <- ideal_tree_mapping
  res$graph <- ideal_tree_graph
  res
}
