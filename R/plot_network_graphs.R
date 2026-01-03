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

#' Plot Static Genre Tree Graph for Publication
#'
#' Creates a static hierarchical tree graph suitable for publication.
#' Supports horizontal and vertical layouts with automatic label rotation.
#'
#' @param graph An igraph object representing the hierarchical structure
#' @param sizes_lookup Named numeric vector. Node sizes lookup. Tag names as names.
#' @param fill_lookup Named character vector. Node fill colors lookup.
#' @param layout Character. Either "horizontal" or "vertical".
#' @param width Numeric or NULL. Width of the plot in pixels. If NULL or too
#'   small, automatically calculated to prevent label overlap.
#' @param height Numeric or NULL. Height of the plot in pixels. If NULL or too
#'   small, automatically calculated to prevent label overlap.
#' @param font_size Numeric. Font size for labels in pt.
#' @param horizontal_label_levels Numeric or NULL. For vertical layout only:
#'   number of hierarchy levels (from root) to display with horizontal labels.
#'   If NULL, automatically detects levels without overlap.
#' @param spacing_x Numeric. Spacing between nodes in pixels along the x-axis.
#'   Default is 20. Use smaller values to make the plot more compact horizontally,
#'   larger values to spread it out.
#' @param spacing_y Numeric. Spacing between nodes in pixels along the y-axis.
#'   Default is 20. Use smaller values to make the plot more compact vertically,
#'   larger values to spread it out.
#' @param margin_left,margin_right,margin_top,margin_bottom Numeric. Margins
#'   in pixels.
#' @param output_file Character or NULL. If provided, saves the plot as PNG.
#'
#' @details
#' Static features for publication:
#' - Horizontal layout: labels displayed horizontally, splitting nodes on left,
#'   leaf nodes on right
#' - Vertical layout: labels rotated 90 degrees where needed to prevent overlap,
#'   horizontal where space permits. Splitting nodes above, leaf nodes below.
#' - Links displayed in 80% grey with thickness based on edge weight
#' - Splitting node labels in bold
#' - All labels same size and black color
#' - Dimensions auto-adjusted to prevent label overlap
#'
#' @return An r2d3 htmlwidget object. If output_file is provided, also saves
#'   the plot as PNG.
#'
#' @examples
#' \dontrun{
#' plot_static_tree_graph(
#'   graph, sizes_lookup, fill_lookup,
#'   layout = "vertical",
#'   horizontal_label_levels = 2,
#'   output_file = "tree_plot.png"
#' )
#' }
#'
#' @export
plot_static_tree_graph <- function(
  graph,
  sizes_lookup,
  fill_lookup,
  layout = c("horizontal", "vertical"),
  width = NULL,
  height = NULL,
  font_size = 10,
  horizontal_label_levels = NULL,
  spacing_x = 20,
  spacing_y = 20,
  margin_left = 100,
  margin_right = 100,
  margin_top = 50,
  margin_bottom = 50,
  output_file = NULL
) {
  layout <- match.arg(layout)

  root <- get_graph_root(graph)
  sorted_hierarchy <- get_sorted_hierarchy(
    graph,
    root,
    sizes_lookup,
    fill_lookup
  )

  dims <- get_static_tree_dimensions(
    graph,
    layout,
    width,
    height,
    font_size,
    margin_left,
    margin_right,
    margin_top,
    margin_bottom
  )
  width <- dims$width
  height <- dims$height

  weights_lookup <- get_weights_lookup(graph)

  data_d3 <- list(
    tree = sorted_hierarchy,
    weights = weights_lookup,
    layout = layout,
    width = width,
    height = height,
    font_size = font_size,
    horizontal_label_levels = horizontal_label_levels,
    spacing_x = spacing_x,
    spacing_y = spacing_y,
    margin_left = margin_left,
    margin_right = margin_right,
    margin_top = margin_top,
    margin_bottom = margin_bottom
  )

  script_file <- "d3/static_tree_plot.js"

  widget <- r2d3::r2d3(
    data = data_d3,
    script = system.file(script_file, package = "GenreDiscourseAnalysis"),
    d3_version = "6",
    width = width,
    height = height
  )

  if (!is.null(output_file)) {
    save_tree_widget_as_png(widget, output_file, width, height)
  }

  widget
}

save_tree_widget_as_png <- function(widget, output_file, width, height) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop(
      "Package 'htmlwidgets' is required for PNG export. ",
      "Install it with: install.packages('htmlwidgets')"
    )
  }
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop(
      "Package 'webshot2' is required for PNG export. ",
      "Install it with: install.packages('webshot2')"
    )
  }

  temp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(widget, temp_html, selfcontained = TRUE)

  webshot2::webshot(
    temp_html,
    file = output_file,
    vwidth = width,
    vheight = height,
    zoom = 2,
    delay = 0.5
  )

  unlink(temp_html)
  message("Saved plot to: ", output_file)
}

get_static_tree_dimensions <- function(
  graph,
  layout,
  width,
  height,
  font_size,
  margin_left,
  margin_right,
  margin_top,
  margin_bottom
) {
  min_dims <- calculate_min_tree_dimensions(
    graph,
    layout,
    font_size,
    margin_left,
    margin_right,
    margin_top,
    margin_bottom
  )

  final_width <- if (is.null(width)) {
    min_dims$min_width
  } else {
    max(width, min_dims$min_width)
  }

  final_height <- if (is.null(height)) {
    min_dims$min_height
  } else {
    max(height, min_dims$min_height)
  }

  list(width = ceiling(final_width), height = ceiling(final_height))
}

calculate_min_tree_dimensions <- function(
  graph,
  layout,
  font_size,
  margin_left,
  margin_right,
  margin_top,
  margin_bottom
) {
  nodes <- igraph::V(graph)$name
  depths <- get_distances_to_root(graph)

  nodes_per_depth <- table(depths$hierarchy_level)
  max_nodes_at_depth <- max(nodes_per_depth)
  total_depths <- length(unique(depths$hierarchy_level))
  max_label_length <- max(nchar(nodes))

  char_width <- font_size * 0.6
  char_height <- font_size * 1.2
  label_pixel_length <- max_label_length * char_width
  min_label_spacing <- 2

  if (layout == "horizontal") {
    min_height <- max_nodes_at_depth *
      (char_height + min_label_spacing) +
      margin_top +
      margin_bottom
    min_width <- total_depths *
      (label_pixel_length + 40) +
      margin_left +
      margin_right
  } else {
    min_width <- max_nodes_at_depth *
      (char_height + min_label_spacing) +
      margin_left +
      margin_right
    min_height <- total_depths *
      (label_pixel_length + 40) +
      margin_top +
      margin_bottom
  }

  list(min_width = min_width, min_height = min_height)
}
