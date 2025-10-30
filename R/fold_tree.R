#' Tune genre-tree folding and return candidate solutions
#'
#' This function folds a hierarchical genre graph from the leaves up,
#' producing solutions for a grid of minimum-genre sizes. For each
#' solution it computes a weighted Gini across hierarchy levels and
#' returns candidate local minima and a suggested solution in the
#' requested range.
#'
#' The function uses parallel processing to speed up computation across
#' the grid of min_n values. Each min_n value is processed independently.
#'
#' @param long data.frame with columns track.s.id, tag_name, tag_count.
#' @param graph igraph object representing full genre hierarchy.
#' @param optimal_solution_range_n_metagenres integer vector length 2.
#' @param min_n_grid_min integer starting minimum n for grid.
#' @param min_n_grid_max integer upper bound for grid or integer().
#' @param min_n_grid_step integer step for grid.
#' @param root character root genre name.
#' @param n_workers integer number of parallel workers. If NULL (default),
#'   uses (number of cores - 1).
#' @return list with solutions, ginis, suggested_solution, candidates, plot.
tune_tree_folding <- function(
  long,
  graph,
  optimal_solution_range_n_metagenres,
  min_n_grid_min = 100,
  min_n_grid_max = integer(),
  min_n_grid_step = 20,
  root = 'POPULAR MUSIC',
  n_workers = NULL
) {
  # Check for required packages
  if (
    !requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("furrr", quietly = TRUE)
  ) {
    stop(
      "Packages 'future' and 'furrr' required for parallel processing.\n",
      "Install with: install.packages(c('future', 'furrr'))"
    )
  }

  if (
    !all(
      c('track.s.id', 'tag_name', 'tag_count') %in%
        colnames(long)
    )
  ) {
    stop('tune_tree_folding: long must have track.s.id, tag_name, tag_count')
  }

  graph_root <- get_subgraph(graph, root)
  search_grid <- get_search_grid(
    min_n_grid_min,
    min_n_grid_max,
    min_n_grid_step,
    long,
    root
  )

  # Pre-compute read-only values that don't change
  sizes_cache <- get_sizes_lookup(long, root)
  hierarchy_cache <- get_distances_to_root(graph_root)

  # Determine number of workers
  # For small grids, parallel overhead isn't worth it
  if (is.null(n_workers)) {
    if (length(search_grid) <= 3) {
      n_workers <- 1 # Sequential for small grids
    } else {
      n_workers <- min(length(search_grid), max(1, parallel::detectCores() - 1))
    }
  }

  # Set up parallel backend only if using multiple workers
  if (n_workers > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = n_workers)
  }

  if (n_workers > 1) {
    message(sprintf(
      'Running parallel tuning with %d workers for %d min_n values...',
      n_workers,
      length(search_grid)
    ))
  }

  # Run folding in parallel for each min_n
  # Load package in each worker so internal functions are available
  if (n_workers > 1) {
    results <- suppressWarnings({
      furrr::future_map(
        search_grid,
        function(min_n_val) {
          # Load package quietly in worker (suppresses all output and warnings)
          suppressMessages(suppressWarnings(
            try(devtools::load_all(quiet = TRUE), silent = TRUE)
          ))

          list(
            min_n = min_n_val,
            solution = fold_genre_tree_bottom_to_top(
              long = long,
              graph_connected = graph_root,
              min_n = min_n_val,
              root = root,
              sizes_cache = sizes_cache,
              hierarchy_cache = hierarchy_cache
            )
          )
        },
        .options = furrr::furrr_options(
          seed = TRUE,
          globals = TRUE,
          packages = c("devtools", "dplyr", "igraph", "DescTools")
        ),
        .progress = TRUE
      )
    })
  } else {
    # Sequential execution for small grids (no parallel overhead)
    results <- lapply(search_grid, function(min_n_val) {
      list(
        min_n = min_n_val,
        solution = fold_genre_tree_bottom_to_top(
          long = long,
          graph_connected = graph_root,
          min_n = min_n_val,
          root = root,
          sizes_cache = sizes_cache,
          hierarchy_cache = hierarchy_cache
        )
      )
    })
  }

  # Process results into expected format
  res_list <- list()
  ginis <- data.frame(
    n_metagenres = integer(length(results)),
    min_n = integer(length(results)),
    weighted_gini = numeric(length(results))
  )

  for (i in seq_along(results)) {
    min_n_val <- results[[i]]$min_n
    sol <- results[[i]]$solution
    sol_name <- as.character(min_n_val)

    ginis[i, ] <- list(
      n_metagenres = nrow(sol$n_songs),
      min_n = min_n_val,
      weighted_gini = get_gini_coefficient(sol$n_songs, hierarchy_cache)
    )
    res_list[[sol_name]] <- sol
  }

  # Remove duplicate solutions (same n_metagenres)
  if (length(results) > 1) {
    for (i in 2:length(results)) {
      if (ginis$n_metagenres[i] == ginis$n_metagenres[i - 1]) {
        sol_name_prev <- as.character(ginis$min_n[i - 1])
        res_list[[sol_name_prev]] <- NULL
      }
    }
  }

  # Check for early termination (NaN Gini)
  nan_idx <- which(is.nan(ginis$weighted_gini))
  if (length(nan_idx) > 0) {
    first_nan <- min(nan_idx)
    message('Stopping search: Gini computation not possible anymore')
    ginis <- ginis[1:first_nan, ]
    # Note: We don't filter res_list here because duplicate removal may have
    # already removed earlier solutions, and we want to keep what we have
  }

  tuning <- list(
    solutions = res_list,
    ginis = ginis,
    root = root
  )

  tuning <- handle_non_gini_solutions(tuning)
  tuning$candidates <- get_local_minima_candidates(tuning$ginis)
  tuning$suggested_solution <- if (nrow(tuning$ginis) > 0) {
    get_suggested_solution(tuning, optimal_solution_range_n_metagenres)
  } else {
    # When ginis is empty, return first solution if it exists
    if (length(tuning$solutions) > 0) {
      tuning$solutions[[1]]
    } else {
      NULL
    }
  }

  n_meta <- nrow(tuning$suggested_solution$n_songs)
  tuning$plot <- plot_tuning_results(
    tuning$ginis,
    xlimits = c(0, min(max(tuning$ginis$n_metagenres), 50)),
    best_candidate = n_meta
  )

  tuning
}

handle_non_gini_solutions <- function(tuning) {
  only_nan <- tuning$ginis |>
    dplyr::filter(!is.na(weighted_gini)) |>
    nrow() ==
    0
  if (only_nan) {
    tuning <- get_non_gini_solutions(tuning)
  } else {
    tuning <- remove_non_gini_solutions(tuning)
  }
  tuning
}

get_non_gini_solutions <- function(tuning) {
  tuning$ginis <- head(tuning$ginis, 0)
  tuning
}

remove_non_gini_solutions <- function(tuning) {
  tuning$solutions <- head(tuning$solutions, -1)
  tuning$ginis <- head(tuning$ginis, -1)
  tuning
}

get_search_grid <- function(min_n_min, min_n_max, step, long, root) {
  if (length(min_n_max) == 0) {
    sizes <- get_sizes_lookup(long, root)
    upper <- ceiling(max(sizes))
    min_n_max <- upper
  }
  grid <- seq(min_n_min, min_n_max, step)
  if (!min_n_max %in% grid) {
    grid <- c(grid, min_n_max)
  }
  grid
}

tune_by_folding_genre_tree_bottom_to_top <- function(
  long,
  graph_connected,
  min_n_grid,
  root,
  sizes_cache = NULL,
  hierarchy_cache = NULL
) {
  # Use cached values or compute if not provided
  if (is.null(sizes_cache)) {
    sizes_cache <- get_sizes_lookup(long, root)
  }
  if (is.null(hierarchy_cache)) {
    hierarchy_cache <- get_distances_to_root(graph_connected)
  }

  res_list <- list()
  ginis <- data.frame(
    n_metagenres = integer(length(min_n_grid)),
    min_n = integer(length(min_n_grid)),
    weighted_gini = numeric(length(min_n_grid))
  )
  sol_names <- as.character(min_n_grid)
  for (i in seq_along(min_n_grid)) {
    message(sprintf('Folding tree for minimum n = %d...', min_n_grid[i]))
    temp <- fold_genre_tree_bottom_to_top(
      long,
      graph_connected,
      min_n_grid[i],
      root,
      sizes_cache = sizes_cache,
      hierarchy_cache = hierarchy_cache
    )
    # Use cached hierarchy for Gini computation
    ginis[i, ] <- list(
      n_metagenres = nrow(temp$n_songs),
      min_n = min_n_grid[i],
      weighted_gini = get_gini_coefficient(temp$n_songs, hierarchy_cache)
    )
    res_list[[sol_names[i]]] <- temp
    if (i > 1) {
      if (ginis$n_metagenres[i] == ginis$n_metagenres[i - 1]) {
        res_list[[sol_names[i - 1]]] <- NULL
      }
    }
    if (is.nan(ginis$weighted_gini[i])) {
      message('Stopping search: Gini computation not possible anymore')
      ginis <- ginis[1:i, ]
      break
    }
  }
  list(solutions = res_list, ginis = ginis)
}

fold_genre_tree_bottom_to_top <- function(
  long,
  graph_connected,
  min_n,
  root,
  sizes_cache = NULL,
  hierarchy_cache = NULL
) {
  # Use cached values or compute if not provided
  if (is.null(sizes_cache)) {
    sizes_cache <- get_sizes_lookup(long, root)
  }
  if (is.null(hierarchy_cache)) {
    hierarchy_cache <- get_distances_to_root(graph_connected)
  }

  hierarchy <- hierarchy_cache |>
    dplyr::mutate(checked = FALSE)
  current_mapping <- long |>
    dplyr::distinct(tag_name) |>
    dplyr::mutate(genre = tag_name)
  sizes <- sizes_cache
  n_songs <- data.frame(
    genre = names(sizes),
    n = as.numeric(sizes),
    stringsAsFactors = FALSE
  )
  current_graph <- graph_connected
  repeat {
    current <- get_current_genre(hierarchy, n_songs)
    if (is_root_level(current, hierarchy)) {
      break
    }
    current_n <- get_current_genre_n(current, n_songs)
    if (current_n >= min_n) {
      if (
        is_parent_and_remaining_larger_min_n(
          current,
          current_graph,
          n_songs,
          min_n
        ) ||
          is_parent_of_current_genre_root(current, current_graph, root)
      ) {
        current_graph <- fold_new_metagenre_from_graph(current, current_graph)
        hierarchy <- set_checked_flag_current_genre(current, hierarchy)
      } else {
        temp <- integrate_genre_and_siblings_and_children(
          current,
          current_mapping,
          current_graph,
          graph_connected,
          hierarchy,
          sizes
        )
        current_mapping <- temp$current_mapping
        n_songs <- temp$n_songs
        hierarchy <- temp$hierarchy
      }
    } else {
      temp <- integrate_genre_and_siblings_and_children(
        current,
        current_mapping,
        current_graph,
        graph_connected,
        hierarchy,
        sizes
      )
      current_mapping <- temp$current_mapping
      n_songs <- temp$n_songs
      hierarchy <- temp$hierarchy
    }
  }
  metagenre_graph <- get_metagenre_graph(current_mapping, graph_connected, root)
  mapping_out <- current_mapping |> dplyr::rename(metagenre = genre)
  list(
    mapping = mapping_out,
    segmented_graph = current_graph,
    metagenre_graph = metagenre_graph,
    min_n = min_n,
    n_songs = n_songs
  )
}

is_root_level <- function(current_genre, hierarchy) {
  hierarchy |>
    dplyr::filter(tag_name == current_genre) |>
    dplyr::pull(hierarchy_level) ==
    0
}

get_current_genre <- function(hierarchy, n_songs) {
  hierarchy |>
    dplyr::left_join(n_songs, by = dplyr::join_by('tag_name' == 'genre')) |>
    dplyr::filter(!checked) |>
    dplyr::arrange(dplyr::desc(hierarchy_level), dplyr::desc(n)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(tag_name)
}

get_current_genre_n <- function(current_genre, n_songs) {
  cur <- n_songs |>
    dplyr::filter(genre == current_genre) |>
    dplyr::pull(n)
  if (length(cur) == 0) {
    cur <- 0
  }
  cur
}

is_parent_and_remaining_larger_min_n <- function(
  genre,
  current_graph,
  n_songs,
  min_n
) {
  supergenre <- names(igraph::neighbors(current_graph, genre, mode = "out"))
  if (length(supergenre) == 0) {
    return(NULL)
  }
  siblings <- names(igraph::neighbors(current_graph, supergenre, mode = 'in'))
  siblings <- siblings[siblings != genre]
  remaining <- c(supergenre, siblings)
  dplyr::filter(n_songs, genre %in% remaining) |>
    dplyr::pull(n) |>
    sum() >=
    min_n
}

is_parent_of_current_genre_root <- function(genre, current_graph, root) {
  parent <- names(igraph::neighbors(current_graph, genre, mode = "out"))
  if (length(parent) == 0) {
    return(FALSE)
  }
  parent == root
}

integrate_genre_and_siblings_and_children <- function(
  current_genre,
  current_mapping,
  current_graph,
  graph_connected,
  hierarchy,
  sizes
) {
  parent <- names(igraph::neighbors(current_graph, current_genre, mode = "out"))
  children <- names(igraph::neighbors(current_graph, parent, mode = "in"))
  current_metagenres <- union(
    unique(current_mapping$genre),
    hierarchy$tag_name[hierarchy$checked == FALSE]
  )
  current_metatree <- igraph::induced_subgraph(
    graph_connected,
    current_metagenres
  )
  grandchildren <- lapply(children, function(ch) {
    igraph::neighbors(current_metatree, ch, mode = "in") |> names()
  }) |>
    unlist()
  nodes_to_fold <- c(children, grandchildren)
  current_mapping <- current_mapping |>
    dplyr::mutate(genre = ifelse(genre %in% nodes_to_fold, parent, genre))
  sizes_df <- data.frame(
    tag_name = names(sizes),
    size = as.numeric(sizes),
    stringsAsFactors = FALSE
  )
  n_songs <- current_mapping |>
    dplyr::left_join(sizes_df, by = dplyr::join_by(tag_name == tag_name)) |>
    dplyr::group_by(genre) |>
    dplyr::summarise(n = sum(size, na.rm = TRUE), .groups = 'drop')
  hierarchy <- hierarchy |>
    dplyr::mutate(checked = ifelse(tag_name %in% nodes_to_fold, TRUE, checked))
  list(
    current_mapping = current_mapping,
    n_songs = n_songs,
    hierarchy = hierarchy
  )
}

fold_new_metagenre_from_graph <- function(current_genre, current_graph) {
  parent <- names(igraph::neighbors(current_graph, current_genre, mode = "out"))
  if (length(parent) == 0) {
    return(current_graph)
  }
  if (length(parent) > 1) {
    stop("Genre has more than one parent.")
  }
  edge_id <- igraph::get_edge_ids(
    current_graph,
    c(current_genre, parent),
    directed = TRUE
  )
  igraph::delete_edges(current_graph, igraph::E(current_graph)[edge_id])
}

set_checked_flag_current_genre <- function(current_genre, hierarchy) {
  hierarchy |>
    dplyr::mutate(checked = ifelse(tag_name == current_genre, TRUE, checked))
}

get_metagenre_graph <- function(mapping, graph, root) {
  metagenres <- unique(mapping$genre)
  metagenres <- union(metagenres, root)
  igraph::induced_subgraph(graph, igraph::V(graph)[name %in% metagenres])
}

get_gini_coefficient <- function(n_songs, distances_to_root) {
  n_songs_hierarchy <- n_songs |>
    dplyr::left_join(
      distances_to_root,
      by = dplyr::join_by('genre' == 'tag_name')
    )
  ginis <- n_songs_hierarchy |>
    dplyr::group_by(hierarchy_level) |>
    dplyr::mutate(relfreq = n / sum(n)) |>
    dplyr::summarize(gini = DescTools::Gini(relfreq))
  counts <- n_songs_hierarchy |>
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
  pool <- ginis |>
    dplyr::arrange(n_metagenres, -min_n) |>
    dplyr::distinct(weighted_gini, n_metagenres, .keep_all = TRUE)
  pool <- dplyr::bind_rows(
    data.frame(weighted_gini = 1, n_metagenres = 1),
    pool
  ) |>
    dplyr::bind_rows(data.frame(weighted_gini = 0, n_metagenres = Inf))
  from_left <- which(diff(pool$weighted_gini) < 0) + 1
  from_right <- nrow(pool) - which(diff(rev(pool$weighted_gini)) < 0)
  candidates <- intersect(from_left, from_right)
  pool[candidates, ]
}

get_suggested_solution <- function(tuning, solution_range) {
  suggestions <- tuning$candidates |>
    dplyr::filter(
      n_metagenres >= solution_range[1] &
        n_metagenres <= solution_range[2]
    ) |>
    dplyr::arrange(-n_metagenres)
  if (nrow(suggestions) == 0) {
    suggestions <- tuning$candidates |>
      dplyr::filter(n_metagenres <= solution_range[2]) |>
      dplyr::arrange(-n_metagenres)
  }
  if (nrow(suggestions) == 0) {
    suggestions <- tuning$candidates |>
      dplyr::arrange(n_metagenres)
  }
  if (nrow(suggestions) == 0) {
    suggestions <- tuning$ginis |>
      dplyr::filter(
        n_metagenres >= solution_range[1] &
          n_metagenres <= solution_range[2]
      ) |>
      dplyr::arrange(weighted_gini)
  }
  if (nrow(suggestions) == 0) {
    suggestions <- tuning$ginis |>
      dplyr::arrange(weighted_gini)
  }
  n_meta <- suggestions[1, ]$n_metagenres
  get_n_metagenre_solution_from_tuning_results(tuning, n_meta)
}

get_n_metagenre_solution_from_tuning_results <- function(tuning, n_genres) {
  min_n_solution <- tuning$ginis |>
    dplyr::filter(n_metagenres == n_genres) |>
    dplyr::arrange(-min_n) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(min_n) |>
    toString()
  tuning$solutions[[min_n_solution]]
}

get_ideal_tree_from_tuning <- function(ideal_tree_tuning, graph) {
  mapping <- lapply(ideal_tree_tuning, function(m) {
    m$suggested_solution$mapping
  }) |>
    do.call(bind_rows, .)
  submetagenres <- unique(mapping$metagenre)
  delete_ids <- igraph::V(graph)[!names(V(graph)) %in% submetagenres]
  ideal_graph <- igraph::delete_vertices(graph, delete_ids)
  list(mapping = mapping, graph = ideal_graph)
}
