#' Plot Network Graph
#'
#' Creates either a static or interactive hierarchical network graph
#' using D3.js through the r2d3 package.
#'
#' @param graph An igraph object representing the hierarchical structure
#' @param sizes_lookup Named numeric vector. Node sizes lookup. Tag names as names.
#' @param fill_lookup Named character vector. Node fill colors lookup. Tag names as names.
#' @param nodesizes Numeric. Scaling factor for node sizes
#' @param margin_left,margin_right,margin_top,margin_bottom Numeric. Margins
#'  in pixels (ignored when interactive = TRUE, uses 100px each)
#' @param height Numeric. Height of the plot in pixels
#' @param min_font_size,max_font_size Numeric. Font size range for node labels
#'
#' @details
#'Interactive features include:
#' - Fixed height of 967px with scrolling
#' - Initially shows only root and first sublevel
#' - Click nodes or labels to expand/collapse their children
#' - Bold, underlined blue labels indicate expandable nodes
#' - +/- symbols show expand/collapse state
#' - Automatic visual feedback with hover effects
#' - Built-in legend with instructions
#' - Compatible with D3 version 6
#'
#' @return An r2d3 htmlwidget object
#'
#' @examples
#' \dontrun{
#' # Static plot
#' plot_network_graph(graph, mapping, interactive = FALSE)
#'
#' # Interactive plot
#' plot_network_graph(graph, mapping, interactive = TRUE)
#' }
#'
#' @export
plot_network_graph <- function(
  graph,
  sizes_lookup,
  fill_lookup,
  nodesizes = 1,
  height = 1000,
  margin_left = 150,
  margin_right = 350,
  margin_top = 0,
  margin_bottom = 0,
  min_font_size = 22,
  max_font_size = 36
) {
  root <- get_graph_root(graph)
  sorted_hierarchy <- get_sorted_hierarchy(
    graph,
    root,
    sizes_lookup,
    fill_lookup
  )

  data_d3 <- list(
    tree = sorted_hierarchy,
    margin_left = margin_left,
    margin_right = margin_right,
    margin_top = margin_top,
    margin_bottom = margin_bottom,
    min_font_size = min_font_size,
    max_font_size = max_font_size
  )

  script_file <- "d3/interactive_network_plot.js"

  r2d3::r2d3(
    data = data_d3,
    script = system.file(script_file, package = "GenreDiscourseAnalysis"),
    d3_version = "6",
    height = height,
    width = "100%",
    options = list(
      container = "div",
      viewer = NULL
    )
  )
}

get_sorted_hierarchy <- function(
  graph,
  root,
  sizes_lookup,
  fill_lookup
) {
  subgraph <- get_subgraph(graph, root)
  edges <- igraph::as_data_frame(subgraph, what = "edges")
  hierarchy <- create_hierarchy(root$name, edges, sizes_lookup, fill_lookup)
  hierarchy
}

create_hierarchy <- function(node_id, edges, sizes_lookup, fill_lookup) {
  children <- edges[edges$to == node_id, "from"]
  if (length(children) == 0) {
    list(
      name = as.character(node_id),
      size = as.double(sizes_lookup[[node_id]]),
      fill = as.character(fill_lookup[[node_id]])
    )
  } else {
    list(
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
    )
  }
}


get_sizes_lookup <- function(long, root = "POPULAR MUSIC") {
  # based on relative tag counts per track
  sizes_lookup <- long |>
    dplyr::group_by(track.s.id, tag_name) |>
    dplyr::summarise(size = sum(tag_count, na.rm = TRUE)) |>
    dplyr::mutate(size = size / sum(size)) |>
    dplyr::ungroup() |>
    dplyr::group_by(tag_name) |>
    dplyr::summarise(size = sum(size, na.rm = TRUE))
  sizes_lookup <- setNames(sizes_lookup$size, sizes_lookup$tag_name)
  sizes_lookup <- c(sizes_lookup, setNames(0.001, root))
  sizes_lookup
}

get_fills_lookup <- function(long, root = "POPULAR MUSIC") {
  fill_lookup <- long |>
    dplyr::distinct(tag_name) |>
    dplyr::mutate(fill = "#808080ff")
  fill_lookup <- setNames(fill_lookup$fill, fill_lookup$tag_name)
  fill_lookup <- c(fill_lookup, setNames("#000000ff", root))
}
