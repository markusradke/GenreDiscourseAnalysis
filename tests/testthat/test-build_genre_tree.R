test_that("create_empty_adjacency_matrix creates correct zero matrix", {
  tags <- c("rock", "pop", "jazz")
  adj <- create_empty_adjacency_matrix(tags)

  expect_equal(dim(adj), c(3, 3))
  expect_equal(rownames(adj), tags)
  expect_equal(colnames(adj), tags)
  expect_true(all(adj == 0))
})

test_that("create_tag_combinations_within_tracks creates valid tag combinations", {
  tags <- data.frame(
    track.s.id = c("a", "a", "b", "b"),
    tag_name = c("rock", "pop", "rock", "jazz"),
    tag_count = c(10, 5, 8, 3),
    stringsAsFactors = FALSE
  )

  combinations <- create_tag_combinations_within_tracks(tags)

  # should not have self-combinations
  expect_true(all(combinations$tag_name_i != combinations$tag_name_j))
  # track a should have rock->pop and pop->rock
  track_a_combos <- combinations[combinations$track.s.id == "a", ]
  expect_equal(nrow(track_a_combos), 2)
  expect_true(any(
    track_a_combos$tag_name_i == "rock" & track_a_combos$tag_name_j == "pop"
  ))
  expect_true(any(
    track_a_combos$tag_name_i == "pop" & track_a_combos$tag_name_j == "rock"
  ))
})

test_that("calculate_tag_cooccurrence_matrix calculates weights correctly", {
  tags <- data.frame(
    track.s.id = c(1, 1, 2),
    tag_name = c("rock", "pop", "rock"),
    tag_count = c(10, 5, 8),
    stringsAsFactors = FALSE
  )

  adj <- calculate_tag_cooccurrence_matrix(tags)

  # rock appears in 2 tracks, pop appears in 1 track
  # rock->pop: 1 co-occurrence out of 2 rock appearances = 0.5
  # pop->rock: 1 co-occurrence out of 1 pop appearance = 1.0
  expect_equal(adj["rock", "pop"], 0.5)
  expect_equal(adj["pop", "rock"], 1.0)
  expect_equal(adj["rock", "rock"], 0) # no self-loops
})


test_that("calculate_vote_based_weights calculates proportional weights", {
  genre_i_only <- data.frame(
    track.s.id = c("a", "a", "b"),
    tag_name_i = c("rock", "rock", "rock"),
    tag_count_i = c(10, 10, 8),
    tag_name_j = c("pop", "jazz", "pop"),
    tag_count_j = c(5, 3, 4),
    stringsAsFactors = FALSE
  )

  weights <- calculate_vote_based_weights(genre_i_only)

  expect_true("tag_name_j" %in% colnames(weights))
  expect_true("weight" %in% colnames(weights))
  expect_true("pop" %in% weights$tag_name_j)
  expect_true("jazz" %in% weights$tag_name_j)
  # weights should be positive
  expect_true(all(weights$weight > 0))
})

test_that("remove_weaker_bidirectional_edges removes bidirectional edges correctly", {
  # create a 3x3 adjacency matrix with known values
  adj <- matrix(
    c(
      0,
      0.8,
      0.2,
      0.3,
      0,
      0.4,
      0.1,
      0.6,
      0
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  soft_child <- remove_weaker_bidirectional_edges(adj)

  # A->B: 0.8 >= 0.3, so keep A->B (0.8-0.3=0.5), remove B->A
  expect_equal(soft_child["A", "B"], 0.5)
  expect_equal(soft_child["B", "A"], 0)

  # B->C: 0.4 < 0.6, so keep C->B (0.6-0.4=0.2), remove B->C
  expect_equal(soft_child["B", "C"], 0)
  expect_equal(soft_child["C", "B"], 0.2)

  # A->C vs C->A: 0.2 > 0.1, so keep A->C (0.2-0.1=0.1), remove C->A
  expect_equal(soft_child["A", "C"], 0.1)
  expect_equal(soft_child["C", "A"], 0)
})

test_that("select_strongest_parent_per_node selects strongest parent only", {
  # create child_of matrix where each row represents a node's connections
  child_of <- matrix(
    c(
      0,
      0.8,
      0.3, # A connects to B(0.8) and C(0.3), should choose B
      0.2,
      0,
      0.9, # B connects to A(0.2) and C(0.9), should choose C
      0.1,
      0.4,
      0 # C connects to A(0.1) and B(0.4), should choose B
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(child_of) <- colnames(child_of) <- c("A", "B", "C")

  democratic <- select_strongest_parent_per_node(child_of)

  # A should connect only to B (strongest)
  expect_equal(democratic["A", "B"], 0.8)
  expect_equal(democratic["A", "C"], 0)

  # B should connect only to C (strongest)
  expect_equal(democratic["B", "C"], 0.9)
  expect_equal(democratic["B", "A"], 0)

  # C should connect only to B (strongest)
  expect_equal(democratic["C", "B"], 0.4)
  expect_equal(democratic["C", "A"], 0)
})

test_that("apply_vote_weighting preserves matrix dimensions", {
  tags <- data.frame(
    track.s.id = c("a", "a"),
    tag_name = c("rock", "pop"),
    tag_count = c(10, 5),
    stringsAsFactors = FALSE
  )

  basic_adj <- calculate_tag_cooccurrence_matrix(tags)
  weighted_adj <- apply_vote_weighting(basic_adj, tags)

  expect_equal(dim(weighted_adj), dim(basic_adj))
  expect_equal(rownames(weighted_adj), rownames(basic_adj))
  expect_equal(colnames(weighted_adj), colnames(basic_adj))
  # weighted values should be <= original values (since weights are proportions)
  expect_true(all(weighted_adj <= basic_adj | is.na(weighted_adj)))
})

test_that("build_genre_tree handles small dataset without errors", {
  skip_if_not_installed("igraph")

  tags_long <- data.frame(
    track.s.id = c("a", "a", "b", "b", "c"),
    tag_name = c("rock", "pop", "rock", "jazz", "pop"),
    tag_count = c(10, 5, 8, 3, 7),
    stringsAsFactors = FALSE
  )

  # use temporary directory to avoid polluting workspace
  old_wd <- getwd()
  tmp_dir <- tempdir()
  dir.create(file.path(tmp_dir, "models"), showWarnings = FALSE)
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  # should complete without error
  expect_no_error(build_genre_tree(tags_long, "test", vote_weighted = FALSE))
  expect_no_error(build_genre_tree(
    tags_long,
    "test_weighted",
    vote_weighted = TRUE
  ))

  # check that files were created
  expect_true(file.exists("models/test_weighted_graph.rds"))
  file.remove("models/test_weighted_graph.rds")
})

test_that("performance critical functions handle edge cases", {
  # empty adjacency matrix
  empty_adj <- matrix(0, nrow = 0, ncol = 0)
  expect_no_error(remove_weaker_bidirectional_edges(empty_adj))
  expect_no_error(select_strongest_parent_per_node(empty_adj))

  # single node
  single_adj <- matrix(0, nrow = 1, ncol = 1)
  rownames(single_adj) <- colnames(single_adj) <- "A"
  soft_single <- remove_weaker_bidirectional_edges(single_adj)
  demo_single <- select_strongest_parent_per_node(single_adj)
  expect_equal(dim(soft_single), c(1, 1))
  expect_equal(dim(demo_single), c(1, 1))

  # matrix with all zeros (no connections)
  zero_adj <- matrix(0, nrow = 3, ncol = 3)
  rownames(zero_adj) <- colnames(zero_adj) <- c("A", "B", "C")
  soft_zero <- remove_weaker_bidirectional_edges(zero_adj)
  demo_zero <- select_strongest_parent_per_node(zero_adj)
  expect_true(all(soft_zero == 0))
  expect_true(all(demo_zero == 0))
})
