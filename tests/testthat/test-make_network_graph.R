test_that("get_sizes_lookup computes per-track proportions", {
  long <- data.frame(
    track.s.id = 1:5,
    tag_name = c("rock", "pop", "rock", "jazz", "pop"),
    tag_count = c(10, 5, 15, 8, 20),
    stringsAsFactors = FALSE
  )

  sizes <- get_sizes_lookup(long)

  expect_equal(sizes[["rock"]], 2)
  expect_equal(sizes[["pop"]], 2)
  expect_equal(sizes[["jazz"]], 1)
  expect_equal(sizes[["POPULAR MUSIC"]], 0.001)
})

test_that("get_sizes_lookup accepts custom root", {
  long <- data.frame(
    track.s.id = 1:2,
    tag_name = c("a", "b"),
    tag_count = c(1, 1),
    stringsAsFactors = FALSE
  )

  sizes <- get_sizes_lookup(long, root = "music")
  expect_equal(sizes[["music"]], 0.001)
})

test_that("get_sizes_lookup handles multiple tags per track", {
  long_multi <- data.frame(
    track.s.id = c(1, 1, 2, 2, 3),
    tag_name = c("rock", "pop", "rock", "jazz", "pop"),
    tag_count = c(1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  sizes <- get_sizes_lookup(long_multi)
  expect_equal(sizes[["rock"]], 1)
  expect_equal(sizes[["pop"]], 1.5)
  expect_equal(sizes[["jazz"]], 0.5)
})

test_that("get_fills_lookup returns default fills and root fill", {
  long <- data.frame(
    tag_name = c("rock", "pop"),
    stringsAsFactors = FALSE
  )

  fills <- get_fills_lookup(long)

  expect_true(all(fills[c("rock", "pop")] == "#808080ff"))
  expect_equal(fills[["POPULAR MUSIC"]], "#000000ff")
})

test_that("get_weights_lookup returns NULL when no weights", {
  edges <- data.frame(
    from = c("a", "b"),
    to = c("root", "a"),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(edges, directed = TRUE)

  w <- get_weights_lookup(g)
  expect_null(w)
})

test_that("get_weights_lookup formats keys as 'from->to'", {
  edges <- data.frame(
    from = c("child1", "child2"),
    to = c("root", "child1"),
    weight = c(2.5, 3.0),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(edges, directed = TRUE)

  weights_df <- get_weights_lookup(g)

  map <- setNames(weights_df$weight, weights_df$key)
  expect_equal(map[["child1->root"]], 2.5)
  expect_equal(map[["child2->child1"]], 3.0)
})

test_that("create_hierarchy returns a leaf node when no children", {
  edges <- data.frame(
    from = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )

  sizes <- setNames(1, "leaf")
  fills <- setNames("#ff0000", "leaf")

  node <- create_hierarchy("leaf", edges, sizes, fills)

  expect_equal(node$name, "leaf")
  expect_equal(node$size, 1)
  expect_equal(node$fill, "#ff0000")
  expect_true(is.null(node$children))
})

test_that("create_hierarchy builds nested children correctly", {
  edges <- data.frame(
    from = c("c1", "c2", "c3"),
    to = c("root", "root", "c1"),
    stringsAsFactors = FALSE
  )

  sizes <- c(root = 0.001, c1 = 2, c2 = 3, c3 = 1)
  fills <- c(
    root = "#000000ff",
    c1 = "#111111ff",
    c2 = "#222222ff",
    c3 = "#333333ff"
  )

  h <- create_hierarchy("root", edges, sizes, fills)

  expect_equal(h$name, "root")
  expect_equal(length(h$children), 2)
  child_names <- vapply(
    h$children,
    function(x) x$name,
    FUN.VALUE = character(1)
  )
  expect_true(all(c("c1", "c2") %in% child_names))

  c1 <- h$children[[which(child_names == "c1")]]
  expect_equal(length(c1$children), 1)
  expect_equal(c1$children[[1]]$name, "c3")
})

test_that("integration: sizes and fills produce a hierarchy from edges", {
  long <- data.frame(
    track.s.id = c(1, 1, 2, 3),
    tag_name = c("rock", "pop", "rock", "jazz"),
    tag_count = c(1, 1, 2, 1),
    stringsAsFactors = FALSE
  )

  sizes <- get_sizes_lookup(long)
  fills <- get_fills_lookup(long)

  edges <- data.frame(
    from = c("rock", "pop", "jazz"),
    to = rep("POPULAR MUSIC", 3),
    stringsAsFactors = FALSE
  )

  root_h <- create_hierarchy(
    "POPULAR MUSIC",
    edges,
    sizes,
    fills
  )

  expect_equal(root_h$name, "POPULAR MUSIC")
  expect_equal(length(root_h$children), 3)
  child_names <- vapply(
    root_h$children,
    function(x) x$name,
    FUN.VALUE = character(1)
  )
  expect_true(all(
    c("rock", "pop", "jazz") %in%
      child_names
  ))
})
