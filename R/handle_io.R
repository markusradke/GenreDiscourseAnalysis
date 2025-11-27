export_graph_for_gephi_import <- function(graph, name) {
  nodes_df <- data.frame(
    id = c(1:igraph::vcount(graph)),
    label = igraph::V(graph)$name
  ) |>
    dplyr::mutate(
      label = stringr::str_replace_all(.data$label, "&", "&amp;"),
      label = stringr::str_replace_all(.data$label, "<", "&lt;"),
      label = stringr::str_replace_all(.data$label, ">", "&gt;"),
      label = stringr::str_replace_all(.data$label, "\"", ""),
      label = stringr::str_replace_all(.data$label, "'", "")
    )
  n_edges <- igraph::ecount(graph)
  if (n_edges == 0) {
    message(sprintf("Graph %s has no edges, skipping export.", name))
    return(NULL)
  }
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
    output = sprintf("models/trees/%s.gexf", name)
  )
}

save_feather_with_lists <- function(input, filepath) {
  types <- sapply(input, class)
  list_cols <- which(types == "list") |> names()
  message("Converting list cols to binary format...")
  if (length(list_cols > 0)) {
    message(sprintf("List cols found: %s", paste(list_cols, collapse = ", ")))
    for (i in seq_along(list_cols)) {
      message(sprintf("Converting col %d of %d", i, length(list_cols)))
      input[[list_cols[i]]] <- lapply(input[[list_cols[i]]], serialize, NULL)
    }
  } else {
    message("No list cols found.")
  }
  arrow::write_feather(input, filepath)
  message(sprintf("File saved as %s.", filepath))
}

read_feather_with_lists <- function(filepath) {
  output <- arrow::read_feather(filepath)
  types <- vapply(output, function(x) class(x)[1], character(1))
  list_cols <- which(types == "arrow_binary") |> names()
  if (length(list_cols > 0)) {
    message("Converting binary format cols to list...")
    for (i in seq_along(list_cols)) {
      message(sprintf("Converting col %d of %d", i, length(list_cols)))
      output[[list_cols[i]]] <- lapply(output[[list_cols[i]]], unserialize)
    }
  } else {
    message("No binary format cols found.")
  }
  message(sprintf("File read: %s", filepath))
  output
}


save_tuning <- function(tune, platform = "") {
  message("Saving tuning results...")
  saveRDS(tune, sprintf("models/metagenres/tune_%s_metagenres.rds", platform))
  saveRDS(
    tune$suggested_solution,
    sprintf("models/metagenres/metagenres_%s_suggested_solution.rds", platform)
  )
  saveRDS(
    tune$plot,
    sprintf("models/metagenres/tune_%s_gini_plot.rds", platform)
  )
  saveRDS(
    get_tuning_metadata(tune),
    sprintf("models/metagenres/tune_%s_metadata.rds", platform)
  )
}

get_tuning_metadata <- function(tune) {
  list(
    min_min_n = min(tune$ginis$min_n),
    max_min_n = max(tune$ginis$min_n),
    step = unique(diff(tune$ginis$min_n)),
    min_n_metagenre = min(tune$ginis$n_metagenres),
    max_n_metagenre = max(tune$ginis$n_metagenres),
    optimal_min_n = tune$suggested_solution$min_n,
    optimal_n_metagenres = tune$suggested_solution$n_metagenres
  )
}
