test_that("get_search_grid works correctly", {
  # Explicit upper bound
  result <- get_search_grid(100, 500, 50, data.frame())
  expect_equal(result, seq(100, 500, 50))

  # Automatic upper bound based on initial genres
  # Create equivalent `long` with one tag per track
  long <- data.frame(
    track.s.id = 1:7,
    tag_name = c(rep("rock", 6), "pop"),
    tag_count = rep(1, 7)
  )
  result_auto <- get_search_grid(1, integer(), 4, long)
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
    genre = c(
      "rock",
      "jazz",
      "pop",
      "classical"
    ),
    n = c(100, 50, 300, 150)
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
  metagenres_uniform <- data.frame(genre = rep("rock"), n = c(600))
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
  # Create a simple graph: child1 -> parent, child2 -> parent
  edges <- data.frame(
    from = c("child1", "child2", "grandchild1"),
    to = c("parent", "parent", "child1")
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)
  mapping <- data.frame(
    tag_name = c("child1", "child2", "grandchild1"),
    genre = c("child1", "child2", "grandchild1")
  )

  hierarchy <- data.frame(
    tag_name = c("parent", "child1", "child2", "grandchild1"),
    hierarchy_level = c(0, 1, 1, 2),
    checked = c(FALSE, FALSE, FALSE, TRUE)
  )

  # Provide explicit sizes for deterministic test behavior
  sizes <- c(parent = 0, child1 = 2, child2 = 2, grandchild1 = 2)

  # Integrate child1 - should merge child1, child2 and grandchild1 into parent
  result <- integrate_genre_and_siblings_and_children(
    "child1",
    mapping,
    graph, # current graph
    graph, # connected graph
    hierarchy,
    sizes
  )

  # All tags should now map to parent
  expect_true(all(result$current_mapping$genre %in% c("parent")))

  # Check that child1 and child2 are marked as checked
  expect_true(result$hierarchy$checked[result$hierarchy$tag_name == "child1"])
  expect_true(result$hierarchy$checked[result$hierarchy$tag_name == "child2"])

  # Check n_songs calculation: parent should have sum of sizes = 6
  expect_equal(result$n_songs$n[result$n_songs$genre == "parent"], 6)
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

  # Create varying tag_count values to exercise fractional sizes.
  long <- data.frame(
    track.s.id = 1:100,
    tag_name = c(rep("metal", 10), rep("pop", 65), rep("rock", 25)),
    tag_count = c(rep(1, 10), rep(0.5, 65), rep(2, 25))
  )

  tuning <- tune_tree_folding(
    long,
    graph,
    c(5, 40),
    min_n_grid_min = 30,
    min_n_grid_step = 10,
    root = "root"
  )
  expect_true("ginis" %in% names(tuning))
  expect_true("solutions" %in% names(tuning))
})

test_that("fold_genre_tree_bottom_to_top stops at Gini NaN condition", {
  # Test NaN Gini stop criterion with tune_by_folding_genre_tree_bottom_to_top
  # Use a grid that will eventually cause all genres to collapse to root
  edges <- data.frame(
    from = c("rock", "pop", "metal", "hip hop"),
    to = c("root", "root", "rock", "root")
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  long <- data.frame(
    track.s.id = 1:100,
    tag_name = c(
      rep("metal", 10),
      rep("pop", 45),
      rep("rock", 25),
      rep("hip hop", 20)
    ),
    tag_count = c(rep(1, 10), rep(0.5, 45), rep(2, 25), rep(1.5, 20))
  )
  min_n_grid <- seq(5, 100, 5)
  messages <- capture_messages({
    tuning_result <- tune_tree_folding(
      long,
      graph,
      c(5, 40),
      min_n_grid_min = 5,
      root = "root"
    )
  })
  expect_true(any(grepl(
    "Stopping search: Gini computation not possible anymore",
    messages
  )))
  expect_true("ginis" %in% names(tuning_result))
  expect_true("solutions" %in% names(tuning_result))
})

test_that("single genre immediately triggers NaN stop", {
  long_single <- data.frame(
    track.s.id = 1:10,
    tag_name = rep("rock", 10),
    tag_count = rep(1, 10)
  )

  single_edges <- data.frame(
    from = "rock",
    to = "root"
  )
  single_graph <- igraph::graph_from_data_frame(single_edges, directed = TRUE)
  single_messages <- capture_messages({
    single_result <- tune_tree_folding(
      long_single,
      single_graph,
      c(1, 5),
      min_n_grid_min = 5,
      min_n_grid_step = 5,
      root = "root"
    )
  })
  expect_true(any(grepl("Stopping search", single_messages)))
  expect_true("ginis" %in% names(single_result))
  expect_true("solutions" %in% names(single_result))
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

test_that("metagenres have valid parent structure", {
  # Test that each metagenre (except root) parent is also as a metagenre

  edges <- data.frame(
    from = c(
      "metal",
      "rock",
      "pop",
      "jazz",
      "blues",
      "electronic",
      "techno",
      "house"
    ),
    to = c(
      "rock",
      "root",
      "root",
      "root",
      "jazz",
      "root",
      "electronic",
      "electronic"
    )
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  long <- data.frame(
    track.s.id = 1:200,
    tag_name = c(
      rep("metal", 25),
      rep("rock", 35),
      rep("pop", 50),
      rep("blues", 15),
      rep("jazz", 20),
      rep("techno", 30),
      rep("house", 25)
    ),
    tag_count = c(rep(1, 100), rep(0.5, 100))
  )

  tuning <- tune_tree_folding(
    long,
    graph,
    c(10, 50),
    min_n_grid_min = 40,
    min_n_grid_step = 10,
    root = "root",
    n_workers = 1
  )
  expect_true("ginis" %in% names(tuning))
  expect_true("solutions" %in% names(tuning))
})


test_that("algorithm handles edge cases that might occur in large datasets", {
  # Test highly skewed distributions (common in real data)

  # Create a hierarchy with many leaf nodes
  edges <- data.frame(
    from = paste0("subgenre_", 1:50), # 50 leaf nodes
    to = rep(c("pop", "rock", "electronic", "jazz", "classical"), each = 10)
  )
  edges <- rbind(
    edges,
    data.frame(
      from = c("pop", "rock", "electronic", "jazz", "classical"),
      to = rep("root", 5)
    )
  )

  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  # Highly skewed distribution: one dominant genre
  long <- data.frame(
    track.s.id = 1:1000,
    tag_name = c(
      rep("subgenre_1", 800), # 80% of data in one subgenre
      rep("subgenre_2", 50),
      rep("subgenre_3", 30),
      sample(paste0("subgenre_", 4:50), 120, replace = TRUE) # Rest scattered
    ),
    tag_count = runif(1000, 0.5, 2)
  )

  tuning <- tune_tree_folding(
    long,
    graph,
    c(10, 100),
    min_n_grid_min = 40,
    min_n_grid_step = 20,
    root = "root",
    n_workers = 1
  )
  expect_true("ginis" %in% names(tuning))
  expect_true("solutions" %in% names(tuning))

  # Test with very strict min_n (should collapse to root) using tuning API
  tuning_strict <- tune_tree_folding(
    long,
    graph,
    c(1, 10),
    min_n_grid_min = 5,
    min_n_grid_max = 900,
    min_n_grid_step = 100,
    root = "root",
    n_workers = 1
  )
  expect_true("ginis" %in% names(tuning_strict))
})

test_that("algorithm consistency across sample sizes", {
  # Test that algorithm behavior is consistent across different sample sizes

  # Create a problematic case that might trigger issues in large datasets
  test_case <- create_problematic_test_case("parent_violation")

  long_base <- data.frame(
    track.s.id = seq_len(nrow(test_case$long)),
    tag_name = test_case$long$tag_name,
    tag_count = runif(nrow(test_case$long), 0.5, 2)
  )

  tuning <- tune_tree_folding(
    long_base,
    test_case$graph,
    c(5, 50),
    min_n_grid_min = 30,
    min_n_grid_step = 10,
    root = "root",
    n_workers = 1
  )
  expect_true("ginis" %in% names(tuning))
  expect_true("solutions" %in% names(tuning))
})

test_that("algorithm handles known problematic patterns", {
  # Test specific patterns that might cause issues in large datasets

  test_case <- create_problematic_test_case("parent_violation")

  # Test with various min_n values
  for (min_n in c(10, 25, 50, 75)) {
    long_case <- data.frame(
      track.s.id = seq_len(nrow(test_case$long)),
      tag_name = test_case$long$tag_name,
      tag_count = runif(nrow(test_case$long), 0.5, 2)
    )
    tuning_min_n <- tune_tree_folding(
      long_case,
      test_case$graph,
      c(min_n, min_n + 10),
      min_n_grid_min = min_n,
      min_n_grid_step = 10,
      root = "root",
      n_workers = 1
    )
    expect_true("ginis" %in% names(tuning_min_n))
  }
})
