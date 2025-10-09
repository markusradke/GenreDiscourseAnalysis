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

test_that("save and read feather with lists round-trip works", {
  skip_if_not_installed("arrow")

  test_save_and_load_feather <- function(df, is_list_col) {
    suppressMessages(save_feather_with_lists(df, "testfile"))
    expect_true(file.exists("testfile.feather"))

    out <- suppressMessages(read_feather_with_lists("testfile.feather"))
    expect_equal(colnames(df), names(out))
    expect_equal(out$id, df$id)
    if (is_list_col) {
      expect_equal(length(out$listcol), length(df$listcol))
      expect_true(all(vapply(out$listcol, is.list, logical(1))))
      for (i in seq_along(df$listcol)) {
        expect_equal(out$listcol[[i]], df$listcol[[i]])
      }
    }

    file.remove("testfile.feather")
  }

  df <- data.frame(id = 1:3, stringsAsFactors = FALSE)
  test_save_and_load_feather(df, is_list_col = FALSE)
  df$listcol <- list(list(a = 1), list(b = 2:3), list(c = "x"))
  test_save_and_load_feather(df, is_list_col = TRUE)
})
