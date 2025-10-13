plot_network_graph <- function(
  graph,
  mapping,
  sizemode = "initial_genre",
  sortmode = 'size',
  fillmode = 'none',
  height = 950,
  nodesizes = 1,
  margin_left = 150,
  margin_right = 350,
  margin_top = 0,
  margin_bottom = 0,
  minFontSize = 22,
  maxFontSize = 36
) {
  root <- get_graph_root(graph)
  sizes_lookup <- get_sizes_lookup(
    mapping,
    graph,
    sizemode = sizemode,
    factor = nodesizes
  )
  fill_lookup <- get_fill_lookup(mapping, graph, fillmode)
  sorted_hierarchy <- get_sorted_hierarchy_for_plotting(
    graph,
    root,
    sizes_lookup,
    fill_lookup,
    sortmode
  )
  data_d3 <- list(
    tree = sorted_hierarchy,
    margin_left = margin_left,
    margin_right = margin_right,
    margin_top = margin_top,
    margin_bottom = margin_bottom,
    minFontSize = minFontSize,

    maxFontSize = maxFontSize
  )
  r2d3::r2d3(
    data = data_d3,
    script = system.file(
      "d3/network_plot.js",
      package = "GenreDiscourseAnalysis"
    ),
    d3_version = "6",
    height = height
  )
}

get_sizes_lookup <- function(
  mapping,
  full_graph,
  sizemode = "initial_genre",
  factor = 1
) {
  if (sizemode != "initial_genre" & sizemode != "metagenre") {
    stop("sizemode must be 'initial_genre' or 'metagenre'")
  }
  if (!"metagenre" %in% colnames(mapping)) {
    sizemode <- "initial_genre"
  }
  sizes <- mapping %>% dplyr::count(.data[[sizemode]])
  sizes_lookup <- sizes$n * 0.0025 * factor
  names(sizes_lookup) <- sizes[[sizemode]]
  get_padded_lookup_full_graph(sizes_lookup, full_graph, 0)
}

get_fill_lookup <- function(mapping, full_graph, fillmode) {
  if (fillmode == 'metagenres') {
    colored_nodes <- unique(mapping$metagenre)
  } else if (fillmode == 'root') {
    colored_nodes <- unique(mapping$root)
  } else {
    colored_nodes <- c()
  }
  fills <- mapping %>%
    dplyr::distinct(initial_genre) %>%
    dplyr::mutate(
      fill = ifelse(initial_genre %in% colored_nodes, '#c40d20', '#494949ff')
    )
  fill_lookup <- fills$fill
  names(fill_lookup) <- fills$initial_genre
  get_padded_lookup_full_graph(fill_lookup, full_graph, '#494949ff')
}

get_padded_lookup_full_graph <- function(lookup, full_graph, value) {
  all_tags <- igraph::V(full_graph) %>% names()
  zero_example_tags <- all_tags[!all_tags %in% names(lookup)]
  zero_example_tags_lookup <- rep(value, length(zero_example_tags))
  names(zero_example_tags_lookup) <- zero_example_tags
  c(lookup, zero_example_tags_lookup)
}

get_sorted_hierarchy_for_plotting <- function(
  graph,
  root,
  sizes_lookup,
  fill_lookup,
  sortmode
) {
  subgraph <- get_subgraph(graph, root)
  edges <- igraph::as_data_frame(subgraph, what = "edges")
  nodes <- igraph::as_data_frame(subgraph, what = "vertices")
  hierarchy <- create_hierarchy(root$name, edges, sizes_lookup, fill_lookup)
  sort_nested_list(hierarchy, mode = sortmode)
}

create_hierarchy <- function(node_id, edges, sizes_lookup, fill_lookup) {
  children <- edges[edges$to == node_id, 'from']
  if (length(children) == 0) {
    return(list(
      name = as.character(node_id),
      size = as.double(sizes_lookup[[node_id]]),
      fill = as.character(fill_lookup[[node_id]])
    ))
  } else {
    return(list(
      name = as.character(node_id),
      size = as.double(sizes_lookup[[node_id]]),
      fill = as.character(fill_lookup[[node_id]]),
      children = lapply(
        children,
        create_hierarchy,
        edges,
        sizes_lookup,
        fill_lookup
      )
    ))
  }
}

sort_nested_list <- function(x, mode = 'size') {
  has_children <- sapply(x$children, function(child) !is.null(child$children))
  df <- data.frame(
    name = sapply(x$children, function(child) child$name),
    size = sapply(x$children, function(child) child$size),
    has_children = has_children,
    stringsAsFactors = FALSE
  )

  if (mode == 'meta-up_alphabetical') {
    df <- df[order(-df$has_children, df$name), ]
  }
  if (mode == 'meta-up_size') {
    df <- df[order(-df$has_children, -df$size), ]
  }
  if (mode == 'size') {
    df <- df[order(-df$size), ]
  }

  sorted_children <- lapply(df$name, function(name) {
    child_index <- which(sapply(x$children, function(child) child$name == name))
    child <- x$children[[child_index]]
    if (!is.null(child$children) && length(child$children) > 0) {
      child <- sort_nested_list(child, mode)
    }
    return(child)
  })
  return(list(
    name = x$name,
    size = x$size,
    fill = x$fill,
    children = sorted_children
  ))
}
