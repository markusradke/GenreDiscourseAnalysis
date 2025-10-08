test_that("export_graph_for_gephi_import writes GEXF and escapes ampersands", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("rgexf")

  # prepare a small directed graph with labels containing '&'
  g <- igraph::graph_from_edgelist(
    matrix(c(1, 2, 2, 3), ncol = 2, byrow = TRUE),
    directed = TRUE
  )
  igraph::V(g)$name <- c("A & B", "B & C", "C")
  igraph::E(g)$weight <- c(1, 2)

  oldwd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(tmp)

  dir.create("models", showWarnings = FALSE)
  name <- paste0("testgraph_", as.integer(Sys.time()))
  suppressMessages(export_graph_for_gephi_import(g, name))

  gexf_path <- file.path("models", paste0(name, ".gexf"))
  expect_true(file.exists(gexf_path))

  # read file and check that ampersands were escaped
  contents <- readLines(gexf_path, warn = FALSE)
  expect_true(any(grepl("&amp;", contents)))

  # check edges: number of <edge ...> lines should equal ecount
  edge_lines <- grep("<edge ", contents, fixed = TRUE)
  expect_equal(length(edge_lines), igraph::ecount(g))
})
