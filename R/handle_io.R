export_graph_for_gephi_import <- function(graph, name) {
  nodes_df <- data.frame(
    id = c(1:igraph::vcount(graph)),
    label = igraph::V(graph)$name
  ) |>
    dplyr::mutate(label = stringr::str_replace_all(.data$label, "&", "&amp;"))
  edges_df <- as.data.frame(igraph::get.edges(
    graph,
    c(1:igraph::ecount(graph))
  ))
  colnames(edges_df) <- c("source", "target")
  edges_weight <- igraph::E(graph)$weight

  rgexf::write.gexf(
    nodes = nodes_df,
    edges = edges_df,
    edgesWeight = edges_weight,
    defaultedgetype = "directed",
    output = sprintf("models/%s.gexf", name)
  )
}
