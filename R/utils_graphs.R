get_graph_root <- function(graph) {
  roots <- igraph::V(graph)[igraph::degree(graph, mode = "out") == 0]
  if (length(roots) != 1) {
    stop("Graph must have exactly one root")
  }
  roots
}

get_subgraph <- function(graph, node_label) {
  node_id <- igraph::V(graph)[node_label]
  outgoing_edges <- igraph::E(graph)[.from(node_id)]
  graph <- igraph::delete_edges(graph, outgoing_edges)
  components_info <- igraph::components(graph)
  component_id <- components_info$membership[node_id]
  igraph::induced_subgraph(
    graph,
    igraph::V(graph)[components_info$membership == component_id]
  )
}
