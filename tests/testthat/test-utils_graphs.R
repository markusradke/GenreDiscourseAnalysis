test_that("get_graph_root returns the single sink vertex", {
  g <- igraph::make_graph(c("A", "B", "B", "C"), directed = TRUE) # A->B->C
  root <- get_graph_root(g)
  expect_equal(igraph::as_ids(root), "C")
})

test_that("get_graph_root errors when there is not exactly one root", {
  g_multi <- igraph::make_graph(c("A", "B", "C", "D"), directed = TRUE) # A->B and C->D => two sinks
  expect_error(get_graph_root(g_multi))
  g_none <- igraph::make_graph(c("A", "B", "B", "C", "C", "A"), directed = TRUE) # cycle => no sink
  expect_error(get_graph_root(g_none))
})

test_that("get_subgraph returns the component containing the node after removing its outgoing edges", {
  g <- igraph::make_graph(c("A", "B", "B", "C"), directed = TRUE) # A->B->C
  sg <- get_subgraph(g, "B")
  expect_setequal(igraph::as_ids(igraph::V(sg)), c("A", "B"))
  # also works when node_label is numeric index
  sg2 <- get_subgraph(g, 2)
  expect_setequal(igraph::as_ids(igraph::V(sg2)), c("A", "B"))
})

test_that("get_distances_to_root calculates hierarchy levels correctly", {
  # Create hierarchy: music (level 0) -> rock (level 1) -> metal (level 2)
  #                   music (level 0) -> pop (level 1)
  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("metal", "rock", "pop"),
      to = c("rock", "music", "music")
    ),
    directed = TRUE
  )

  distances <- get_distances_to_root(graph)

  expect_true(is.data.frame(distances))
  expect_equal(ncol(distances), 2)
  expect_true(all(c("hierarchy_level", "tag_name") %in% colnames(distances)))
  root_distance <- distances[distances$tag_name == "music", "hierarchy_level"]
  expect_equal(root_distance, 0)
  expect_true(all(distances$hierarchy_level >= 0))
  expect_equal(nrow(distances), igraph::vcount(graph))
})
