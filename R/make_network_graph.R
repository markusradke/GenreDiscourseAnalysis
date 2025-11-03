#' Plot Interactive Genre Tree Graph
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
#' - Edge weights visualized: strong connections are thick and black,
#'   weak connections are thin and light gray
#' - Hover over edges to see exact weight values
#' - Built-in legend with instructions
#' - Compatible with D3 version 6
#'
#' @return An r2d3 htmlwidget object
#'
#' @examples
#' \dontrun{
#' # Static plot
#' plot_interactive_tree_graph(graph, mapping, interactive = FALSE)
#'
#' # Interactive plot
#' plot_interactive_tree_graph(graph, mapping, interactive = TRUE)
#' }
#'
#' @export
plot_interactive_tree_graph <- function(
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

  # Get edge weights for visual styling
  weights_lookup <- get_weights_lookup(graph)

  data_d3 <- list(
    tree = sorted_hierarchy,
    weights = weights_lookup,
    margin_left = margin_left,
    margin_right = margin_right,
    margin_top = margin_top,
    margin_bottom = margin_bottom,
    min_font_size = min_font_size,
    max_font_size = max_font_size
  )

  script_file <- "d3/interactive_tree_plot.js"

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

get_weights_lookup <- function(graph) {
  edges_df <- igraph::as_data_frame(graph, what = "edges")

  if (!"weight" %in% colnames(edges_df)) {
    return(NULL)
  }
  weights_df <- data.frame(
    key = paste(edges_df$from, edges_df$to, sep = "->"),
    weight = edges_df$weight,
    stringsAsFactors = FALSE
  )

  weights_df
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

#' Plot Interactive Genre Genealogy Graph
#'
#' Creates an interactive genealogy graph showing a focused genre with its
#' parent genres on the left and subgenres (children) on the right.
#'
#' @param graph An igraph object representing the genre network
#' @param tree An igraph object representing the tree (for highlighting tree nodes)
#' @param sizes_lookup Named numeric vector. Node sizes lookup. Tag names as names.
#' @param fill_lookup Named character vector. Node fill colors lookup. Tag names as names.
#' @param initial_focus Character. The initial genre to focus on. If NULL, uses root.
#' @param height Numeric. Height of the plot in pixels
#'
#' @details
#' Interactive features include:
#' - Center node shows the focused genre
#' - Parents (genres this genre belongs to) appear on the left
#' - Subgenres (children) appear on the right
#' - Click on any node or label to make it the new focus
#' - "Focus root" button: Changes focus to the root node
#' - "Reset position" button: Re-centers and resets zoom
#' - "Back" button: Returns to previously focused node
#' - Search bar: Type to find and focus on specific genres
#' - Edge threshold slider: Filter edges by weight (tree edges always shown)
#' - Tree nodes are colored green to distinguish them
#' - Edge weights visualized: strong connections are thick and black,
#'   weak connections are thin and light gray
#' - Zoom and pan with mouse wheel and drag
#'
#' @return An r2d3 htmlwidget object
#'
#' @examples
#' \dontrun{
#' plot_interactive_genealogy_graph(graph, tree, sizes_lookup, fill_lookup)
#' }
#'
#' @export
plot_interactive_genealogy_graph <- function(
  graph,
  tree,
  sizes_lookup,
  fill_lookup,
  initial_focus = NULL,
  height = 800
) {
  root <- get_graph_root(graph)

  if (is.null(initial_focus)) {
    initial_focus <- root$name
  }

  genealogy_data <- get_genealogy_structure(
    graph,
    initial_focus,
    sizes_lookup,
    fill_lookup
  )

  # Get tree edges for highlighting
  tree_edges_df <- igraph::as_data_frame(tree, what = "edges")

  data_d3 <- list(
    genealogy = genealogy_data,
    root_name = root$name,
    tree_edges = tree_edges_df,
    height = height
  )

  script_file <- "d3/interactive_genealogy_plot.js"

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

#' Get genealogy structure for a focused node
#'
#' Extracts the parents and children of a focus node with their properties.
#'
#' @param graph An igraph object
#' @param focus_name Character. Name of the node to focus on
#' @param sizes_lookup Named numeric vector of node sizes
#' @param fill_lookup Named character vector of node colors
#'
#' @return A list with components:
#'   - focus: List with name, size, fill of the focused node
#'   - parents: List of parent nodes with name, size, fill, weight
#'   - children: List of child nodes with name, size, fill, weight
#'   - all_nodes: Data frame with all unique nodes
#'   - edges: Data frame with all edges (from, to, weight)
#'
#' @keywords internal
get_genealogy_structure <- function(
  graph,
  focus_name,
  sizes_lookup,
  fill_lookup
) {
  # Get the focus node
  focus_node <- igraph::V(graph)[focus_name]

  # Get parents (nodes that this node points TO)
  parent_edges <- igraph::E(graph)[.from(focus_node)]
  parents <- igraph::ends(graph, parent_edges, names = TRUE)[, 2]

  # Get children (nodes that point FROM other nodes TO this node)
  child_edges <- igraph::E(graph)[.to(focus_node)]
  children <- igraph::ends(graph, child_edges, names = TRUE)[, 1]

  # Get edge weights
  edges_df <- igraph::as_data_frame(graph, what = "edges")

  # Build focus node info
  focus_info <- list(
    name = focus_name,
    size = as.double(sizes_lookup[[focus_name]]),
    fill = as.character(fill_lookup[[focus_name]])
  )

  # Build parent nodes info
  parents_info <- lapply(parents, function(p) {
    # Find weight of edge from focus to parent
    weight <- NA
    if ("weight" %in% colnames(edges_df)) {
      edge_weight <- edges_df[
        edges_df$from == focus_name & edges_df$to == p,
        "weight"
      ]
      if (length(edge_weight) > 0) {
        weight <- edge_weight[1]
      }
    }

    list(
      name = p,
      size = as.double(sizes_lookup[[p]]),
      fill = as.character(fill_lookup[[p]]),
      weight = weight
    )
  })

  # Build children nodes info
  children_info <- lapply(children, function(c) {
    # Find weight of edge from child to focus
    weight <- NA
    if ("weight" %in% colnames(edges_df)) {
      edge_weight <- edges_df[
        edges_df$from == c & edges_df$to == focus_name,
        "weight"
      ]
      if (length(edge_weight) > 0) {
        weight <- edge_weight[1]
      }
    }

    list(
      name = c,
      size = as.double(sizes_lookup[[c]]),
      fill = as.character(fill_lookup[[c]]),
      weight = weight
    )
  })

  # Create comprehensive node and edge lists for full graph traversal
  all_nodes <- data.frame(
    name = igraph::V(graph)$name,
    size = sapply(igraph::V(graph)$name, function(n) {
      as.double(sizes_lookup[[n]])
    }),
    fill = sapply(igraph::V(graph)$name, function(n) {
      as.character(fill_lookup[[n]])
    }),
    stringsAsFactors = FALSE
  )

  list(
    focus = focus_info,
    parents = parents_info,
    children = children_info,
    all_nodes = all_nodes,
    edges = edges_df
  )
}
