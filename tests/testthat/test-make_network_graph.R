test_that("plot_network_graph validates parameters correctly", {
  skip_if_not_installed("r2d3")
  skip_if_not_installed("igraph")

  # Create test data
  mapping <- data.frame(
    track.s.id = 1:3,
    initial_genre = c("rock", "pop", "jazz"),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "music")
    ),
    directed = TRUE
  )

  # Test valid parameters
  expect_no_error({
    plot_network_graph(graph, mapping, interactive = FALSE)
  })

  expect_no_error({
    plot_network_graph(graph, mapping, interactive = TRUE)
  })

  # Test parameter validation
  expect_error(
    plot_network_graph(graph, mapping, sizemode = "invalid"),
    "sizemode must be 'initial_genre' or 'metagenre'"
  )
})

test_that("get_sizes_lookup calculates node sizes correctly", {
  mapping <- data.frame(
    track.s.id = c(1, 1, 2, 2, 3),
    initial_genre = c("rock", "rock", "pop", "pop", "jazz"),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "music")
    ),
    directed = TRUE
  )

  sizes_lookup <- get_sizes_lookup(
    mapping,
    graph,
    sizemode = "initial_genre",
    factor = 1
  )

  # Should return named numeric vector
  expect_type(sizes_lookup, "double")
  expect_true(is.numeric(sizes_lookup))
  expect_true(!is.null(names(sizes_lookup)))

  # Should include all nodes from graph
  graph_nodes <- igraph::V(graph) |> names()
  expect_true(all(graph_nodes %in% names(sizes_lookup)))

  # Rock should be largest (appears twice)
  expect_true(sizes_lookup["rock"] > sizes_lookup["jazz"])
  expect_true(sizes_lookup["pop"] > sizes_lookup["jazz"])
})

test_that("get_sizes_lookup handles metagenre mode", {
  mapping <- data.frame(
    track.s.id = 1:3,
    initial_genre = c("rock", "pop", "jazz"),
    metagenre = c("rock_family", "pop_family", "jazz_family"),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "music")
    ),
    directed = TRUE
  )

  # Should work with metagenre when column exists
  expect_no_error({
    sizes_lookup <- get_sizes_lookup(mapping, graph, sizemode = "metagenre")
  })

  # Should fall back to initial_genre when metagenre column missing
  mapping_no_meta <- mapping[, !colnames(mapping) %in% "metagenre"]
  expect_no_error({
    sizes_lookup <- get_sizes_lookup(
      mapping_no_meta,
      graph,
      sizemode = "metagenre"
    )
  })
})

test_that("get_fill_lookup assigns colors correctly", {
  mapping <- data.frame(
    track.s.id = 1:3,
    initial_genre = c("rock", "pop", "jazz"),
    metagenre = c("rock", "pop", "jazz"),
    root = c("music", "music", "music"),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "music")
    ),
    directed = TRUE
  )

  # Test 'none' fillmode
  fill_lookup_none <- get_fill_lookup(mapping, graph, "none")
  expect_true(all(fill_lookup_none == "#494949ff"))

  # Test 'metagenres' fillmode
  fill_lookup_meta <- get_fill_lookup(mapping, graph, "metagenres")
  # Should have highlighted colors for metagenres
  expect_true(any(fill_lookup_meta == "#c40d20"))

  # Test 'root' fillmode
  fill_lookup_root <- get_fill_lookup(mapping, graph, "root")
  # Root fillmode should highlight nodes that are in the root column
  # In our test data, root column has "music" values, but our initial_genre are rock/pop/jazz
  # So no highlighting should occur unless there's a match
  expect_type(fill_lookup_root, "character")
})

test_that("get_padded_lookup_full_graph pads missing nodes", {
  lookup <- c("rock" = 10, "pop" = 5)

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "music")
    ),
    directed = TRUE
  )

  padded <- get_padded_lookup_full_graph(lookup, graph, 0)

  # Should include all graph nodes
  graph_nodes <- igraph::V(graph) |> names()
  expect_true(all(graph_nodes %in% names(padded)))

  # Should preserve original values
  expect_equal(as.numeric(padded["rock"]), 10)
  expect_equal(as.numeric(padded["pop"]), 5)

  # Should pad missing nodes with default value
  expect_equal(as.numeric(padded["jazz"]), 0)
  expect_equal(as.numeric(padded["music"]), 0)
})

test_that("get_sorted_hierarchy_for_plotting creates valid hierarchy", {
  mapping <- data.frame(
    track.s.id = 1:3,
    initial_genre = c("rock", "pop", "jazz"),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "music")
    ),
    directed = TRUE
  )

  root <- get_graph_root(graph)
  sizes_lookup <- get_sizes_lookup(mapping, graph)
  fill_lookup <- get_fill_lookup(mapping, graph, "none")

  hierarchy <- get_sorted_hierarchy_for_plotting(
    graph,
    root,
    sizes_lookup,
    fill_lookup,
    "size"
  )

  # Should return a list with required fields
  expect_type(hierarchy, "list")
  expect_true("name" %in% names(hierarchy))
  expect_true("size" %in% names(hierarchy))
  expect_true("fill" %in% names(hierarchy))
  expect_true("children" %in% names(hierarchy))

  # Root should be music
  expect_equal(hierarchy$name, "music")

  # Should have children
  expect_type(hierarchy$children, "list")
  expect_true(length(hierarchy$children) > 0)
})

test_that("create_hierarchy builds nested structure correctly", {
  edges <- data.frame(
    from = c("rock", "pop"),
    to = c("music", "music"),
    stringsAsFactors = FALSE
  )

  sizes_lookup <- c("music" = 10, "rock" = 5, "pop" = 3)
  fill_lookup <- c("music" = "#000", "rock" = "#111", "pop" = "#222")

  hierarchy <- create_hierarchy("music", edges, sizes_lookup, fill_lookup)

  # Root node should have correct properties
  expect_equal(hierarchy$name, "music")
  expect_equal(hierarchy$size, 10)
  expect_equal(hierarchy$fill, "#000")

  # Should have children
  expect_type(hierarchy$children, "list")
  expect_equal(length(hierarchy$children), 2)

  # Children should have correct names
  child_names <- sapply(hierarchy$children, function(x) x$name)
  expect_setequal(child_names, c("rock", "pop"))
})

test_that("create_hierarchy handles leaf nodes correctly", {
  edges <- data.frame(
    from = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )

  sizes_lookup <- c("leaf" = 5)
  fill_lookup <- c("leaf" = "#000")

  hierarchy <- create_hierarchy("leaf", edges, sizes_lookup, fill_lookup)

  # Leaf node should not have children
  expect_equal(hierarchy$name, "leaf")
  expect_equal(hierarchy$size, 5)
  expect_equal(hierarchy$fill, "#000")
  expect_false("children" %in% names(hierarchy))
})

test_that("sort_nested_list sorts children correctly", {
  # Create test hierarchy
  hierarchy <- list(
    name = "root",
    size = 100,
    fill = "#000",
    children = list(
      list(name = "small", size = 10, fill = "#111"),
      list(name = "large", size = 50, fill = "#222"),
      list(name = "medium", size = 30, fill = "#333")
    )
  )

  # Test size sorting
  sorted_size <- sort_nested_list(hierarchy, mode = "size")
  child_sizes <- sapply(sorted_size$children, function(x) x$size)
  expect_equal(child_sizes, c(50, 30, 10)) # Should be descending

  # Test that structure is preserved
  expect_equal(sorted_size$name, "root")
  expect_equal(sorted_size$size, 100)
  expect_equal(length(sorted_size$children), 3)
})

test_that("sort_nested_list handles meta-up modes", {
  # Create hierarchy with mixed node types
  hierarchy <- list(
    name = "root",
    size = 100,
    fill = "#000",
    children = list(
      list(name = "leaf1", size = 10, fill = "#111"),
      list(
        name = "parent1",
        size = 50,
        fill = "#222",
        children = list(list(name = "child1", size = 5, fill = "#444"))
      ),
      list(name = "leaf2", size = 30, fill = "#333")
    )
  )

  # Test meta-up_alphabetical
  sorted_alpha <- sort_nested_list(hierarchy, mode = "meta-up_alphabetical")
  child_names <- sapply(sorted_alpha$children, function(x) x$name)
  # Parent should come first (has children), then leaves alphabetically
  expect_equal(child_names[1], "parent1")

  # Test meta-up_size
  sorted_meta_size <- sort_nested_list(hierarchy, mode = "meta-up_size")
  child_names_size <- sapply(sorted_meta_size$children, function(x) x$name)
  # Parent should come first (has children)
  expect_equal(child_names_size[1], "parent1")
})

test_that("functions handle empty graphs gracefully", {
  empty_mapping <- data.frame(
    track.s.id = numeric(0),
    initial_genre = character(0),
    stringsAsFactors = FALSE
  )

  # Create minimal graph with single node
  graph <- igraph::make_empty_graph(n = 1, directed = TRUE)
  igraph::V(graph)$name <- "music"

  expect_no_error({
    sizes_lookup <- get_sizes_lookup(empty_mapping, graph)
  })

  expect_no_error({
    fill_lookup <- get_fill_lookup(empty_mapping, graph, "none")
  })

  # Should handle empty data gracefully
  expect_type(sizes_lookup, "double")
  expect_type(fill_lookup, "character")
})

test_that("interactive parameter affects output correctly", {
  skip_if_not_installed("r2d3")

  mapping <- data.frame(
    track.s.id = 1:2,
    initial_genre = c("rock", "pop"),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(from = c("rock", "pop"), to = c("music", "music")),
    directed = TRUE
  )

  # Both modes should work without error
  expect_no_error({
    static_plot <- plot_network_graph(graph, mapping, interactive = FALSE)
  })

  expect_no_error({
    interactive_plot <- plot_network_graph(graph, mapping, interactive = TRUE)
  })

  # Interactive plot should have fixed height of 800
  # (We can't easily test the actual height without rendering, but we can test the function call)
})
