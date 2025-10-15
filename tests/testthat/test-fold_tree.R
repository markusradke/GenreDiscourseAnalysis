test_that("get_search_grid works correctly", {
  # Explicit upper bound
  result <- get_search_grid(100, 500, 50, data.frame())
  expect_equal(result, seq(100, 500, 50))

  # Automatic upper bound based on initial genres
  initial_genres <- data.frame(
    initial_genre = c(rep("rock", 6), "pop"),
    id = 1:7
  )
  result_auto <- get_search_grid(1, integer(), 4, initial_genres)
  expect_equal(result_auto, c(1, 5, 6))

  result_single <- get_search_grid(100, 100, 50, data.frame())
  expect_equal(result_single, 100)
})

test_that("get_current_genre prioritizes correctly", {
  # Mock hierarchy with different levels and check status
  hierarchy <- data.frame(
    tag_name = c("rock", "metal", "pop", "jazz"),
    hierarchy_level = c(0, 1, 1, 2),
    checked = c(FALSE, FALSE, TRUE, FALSE) # pop already checked
  )

  # Mock n_songs - jazz has most songs but highest level
  n_songs <- data.frame(
    genre = c("rock", "metal", "jazz"),
    n = c(100, 50, 200)
  )

  result <- get_current_genre(hierarchy, n_songs)
  # Should pick jazz (highest hierarchy_level=2, not checked, has songs)
  expect_equal(result, "jazz")

  # Test when highest level is checked
  hierarchy$checked[hierarchy$tag_name == "jazz"] <- TRUE
  result <- get_current_genre(hierarchy, n_songs)
  # Should pick metal (highest available level=1, not checked)
  expect_equal(result, "metal")
})

test_that("get_current_genre_n handles missing genres", {
  n_songs <- data.frame(
    genre = c("rock", "pop"),
    n = c(100, 50)
  )

  result_exist <- get_current_genre_n("rock", n_songs)
  expect_equal(result_exist, 100)
  result_non_exist <- get_current_genre_n("jazz", n_songs)
  expect_equal(result_non_exist, 0)
})

test_that("get_gini_coefficient calculates weighted average correctly", {
  metagenres <- data.frame(
    metagenre = c(
      rep("rock", 4),
      rep("pop", 2),
      rep("jazz", 1),
      rep("classical", 1)
    )
  )
  get_distances_to_root <- data.frame(
    tag_name = c("rock", "pop", "jazz", "classical"),
    hierarchy_level = c(1, 1, 2, 2)
  )
  result <- get_gini_coefficient(metagenres, get_distances_to_root)
  expect_type(result, "double")
  expect_gte(result, 0)
  expect_lte(result, 1)

  # Test edge case: all same genre (perfect inequality)
  metagenres_uniform <- data.frame(metagenre = rep("rock", 10))
  expect_warning(
    result_uniform <- get_gini_coefficient(
      metagenres_uniform,
      get_distances_to_root
    ),
    NA # We expect this might warn or return NaN
  )
})

test_that("get_local_minima_candidates identifies local minima correctly", {
  # Local min at n=4
  ginis <- data.frame(
    n_metagenres = c(2, 3, 4, 5, 6),
    weighted_gini = c(0.8, 0.6, 0.3, 0.5, 0.7),
    min_n = c(100, 200, 300, 400, 500)
  )
  candidates <- get_local_minima_candidates(ginis)
  expect_equal(candidates$n_metagenres, 4)
})

test_that("is_parent_and_remaining_larger_min_n works correctly", {
  # graph: root <- rock <- metal
  edges <- data.frame(
    from = c("rock", "metal", "altrock", "pop"),
    to = c("root", "rock", "rock", "root")
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  # Mock n_songs where folding rock+metal would exceed min_n
  n_songs <- data.frame(
    genre = c("root", "rock", "metal", "altrock", "pop"),
    n = c(0, 19, 20, 1, 0)
  )

  result <- is_parent_and_remaining_larger_min_n("metal", graph, n_songs, 20)
  expect_true(result)
  result <- is_parent_and_remaining_larger_min_n("rock", graph, n_songs, 20)
  expect_false(result)
  result <- is_parent_and_remaining_larger_min_n("root", graph, n_songs, 20)
  expect_null(result)
})

test_that("integrate_genre_and_siblings merges correctly", {
  # Create test graph: parent <- child1, child2
  edges <- data.frame(
    from = c("child1", "child2"),
    to = c("parent", "parent")
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  # Initial mapping with genres
  mapping <- data.frame(
    id = 1:6,
    initial_genre = c("child1", "child1", "child2", "child2", "other", "other"),
    genre = c("child1", "child1", "child2", "child2", "other", "other")
  )

  hierarchy <- data.frame(
    tag_name = c("parent", "child1", "child2", "other"),
    hierarchy_level = c(0, 1, 1, 1),
    checked = c(FALSE, FALSE, FALSE, TRUE)
  )

  # Integrate child1 - should merge child1 and child2 into parent
  result <- integrate_genre_and_siblings("child1", mapping, graph, hierarchy)

  # Check that child1 and child2 are now mapped to parent
  expect_true(all(result$current_mapping$genre[1:4] == "parent"))
  expect_true(all(result$current_mapping$genre[5:6] == "other"))

  # Check that child1 and child2 are marked as checked
  expect_true(result$hierarchy$checked[result$hierarchy$tag_name == "child1"])
  expect_true(result$hierarchy$checked[result$hierarchy$tag_name == "child2"])

  # Check n_songs calculation
  expect_equal(result$n_songs$n[result$n_songs$genre == "parent"], 4)
  expect_equal(result$n_songs$n[result$n_songs$genre == "other"], 2)
})

test_that("get_suggested_solution selects appropriately", {
  ginis <- data.frame(
    n_metagenres = c(3, 5, 7, 10, 15),
    weighted_gini = c(0.8, 0.4, 0.3, 0.5, 0.7),
    min_n = c(100, 200, 300, 400, 500)
  )

  candidates <- data.frame(
    n_metagenres = c(3, 7), # Local minima
    weighted_gini = c(0.8, 0.3),
    min_n = c(100, 300)
  )

  # Mock solutions
  solutions <- list(
    "300" = list(mapping = data.frame(id = 1:7, metagenre = rep("test", 7))),
    "500" = list(mapping = data.frame(id = 1:15, metagenre = rep("test", 15)))
  )

  tuning <- list(
    ginis = ginis,
    candidates = candidates,
    solutions = solutions
  )

  # Test case 1: solution within range
  solution_range <- c(5, 10)
  result1 <- get_suggested_solution(tuning, solution_range)
  expect_equal(nrow(result1$mapping), 7) # Should pick n_metagenres=7

  # Test case 2: no solution in range, pick from below
  solution_range2 <- c(12, 20)
  result2 <- get_suggested_solution(tuning, solution_range2)
  expect_equal(nrow(result2$mapping), 7) # Should pick highest below range (7)
})

test_that("fold_genre_tree_bottom_to_top results look appropriate", {
  # This is an integration test for the main algorithm
  # Create a simple test case

  # Simple graph: root <- rock <- metal
  #                    <- pop
  edges <- data.frame(
    from = c("rock", "pop", "metal"),
    to = c("root", "root", "rock")
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  # Initial genres with sufficient data
  initial_genres <- data.frame(
    id = 1:100,
    initial_genre = c(rep("metal", 10), rep("pop", 65), rep("rock", 25)),
    root = rep("root", 100)
  )

  # Test with min_n that should cause some folding
  result <- fold_genre_tree_bottom_to_top(initial_genres, graph, 30, "root")
  # Verify structure
  expect_true("mapping" %in% names(result))
  expect_true("n_songs" %in% names(result))
  expect_true("min_n" %in% names(result))
  expect_equal(result$min_n, 30)

  # Should have fewer genres than original
  original_genres <- length(unique(initial_genres$initial_genre))
  final_genres <- nrow(result$n_songs)
  expect_lte(final_genres, original_genres)

  # All songs should still be accounted for
  expect_equal(sum(result$n_songs$n), nrow(initial_genres))
})

test_that("fold_genre_tree_bottom_to_top stops at Gini NaN condition", {
  # Test NaN Gini stop criterion with tune_by_folding_genre_tree_bottom_to_top
  # Use a grid that will eventually cause all genres to collapse to root
  edges <- data.frame(
    from = c("rock", "pop", "metal", "hip hop"),
    to = c("root", "root", "rock", "root")
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  # Initial genres with sufficient data
  initial_genres <- data.frame(
    id = 1:100,
    initial_genre = c(
      rep("metal", 10),
      rep("pop", 45),
      rep("rock", 25),
      rep("hip hop", 20)
    ),
    root = rep("root", 100)
  )

  min_n_grid <- seq(5, 100, 5)
  messages <- capture_messages({
    tuning_result <- tune_by_folding_genre_tree_bottom_to_top(
      initial_genres,
      graph,
      min_n_grid,
      "root"
    )
  })
  expect_true(any(grepl(
    "Stopping search: Gini computation was not possible anymore",
    messages
  )))
  expect_lt(nrow(tuning_result$ginis), length(min_n_grid))
  expect_true(is.nan(tail(tuning_result$ginis$weighted_gini, 1)))
  # Should keep all results incl. the NaN one
  expect_equal(tuning_result$solutions |> names(), c("5", "20", "25"))
})

test_that("single genre immediately triggers NaN stop", {
  single_genre_initial <- data.frame(
    id = 1:10,
    initial_genre = rep("rock", 10),
    root = rep("root", 10)
  )

  single_edges <- data.frame(
    from = "rock",
    to = "root"
  )
  single_graph <- igraph::graph_from_data_frame(single_edges, directed = TRUE)
  single_messages <- capture_messages({
    single_result <- tune_by_folding_genre_tree_bottom_to_top(
      single_genre_initial,
      single_graph,
      c(5, 15),
      "root"
    )
  })
  expect_true(any(grepl("Stopping search", single_messages)))
  expect_lte(nrow(single_result$ginis), 2) # Should stop early
  expect_equal(single_result$solutions |> names(), c("5")) # key result
})

# Helper function tests
test_that("handle_non_gini_solutions works correctly", {
  # Test case with all NaN ginis
  tuning_nan <- list(
    ginis = data.frame(
      n_metagenres = c(1, 2),
      weighted_gini = c(NaN, NaN)
    ),
    solutions = list("sol1" = list(), "sol2" = list())
  )

  result_nan <- handle_non_gini_solutions(tuning_nan)
  expect_equal(nrow(result_nan$ginis), 0) # Should remove all ginis
  expect_equal(length(result_nan$solutions), 2) # Should keep solutions

  # Test case with some valid ginis
  tuning_valid <- list(
    ginis = data.frame(
      n_metagenres = c(1, 2, 3),
      weighted_gini = c(0.5, 0.6, NaN)
    ),
    solutions = list("sol1" = list(), "sol2" = list(), "sol3" = list())
  )

  result_valid <- handle_non_gini_solutions(tuning_valid)
  expect_equal(nrow(result_valid$ginis), 2) # Should remove last NaN
  expect_equal(length(result_valid$solutions), 2) # Should remove last solution
})
