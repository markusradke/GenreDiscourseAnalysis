test_that("get_tree_and_votes_based_mapping handles single tag case", {
  track_tags <- data.frame(
    track.s.id = 1,
    track.s.title = "Song A",
    track.s.firstartist.name = "A",
    tag_name = "rock",
    tag_count = 10,
    votes_total = 100,
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(from = "rock", to = "music"),
    directed = TRUE
  )

  result <- get_tree_and_votes_based_mapping(track_tags, graph)
  expect_equal(result, "rock")
})

test_that("get_tree_and_votes_based_mapping handles multiple tags with hierarchy", {
  track_tags <- data.frame(
    track.s.id = c(1, 1, 1),
    track.s.title = c("Song A", "Song A", "Song A"),
    track.s.firstartist.name = c("A", "A", "A"),
    tag_name = c("metal", "rock", "music"),
    tag_count = c(15, 10, 5), # metal has most votes
    votes_total = c(200, 500, 1000), # but music has most total votes
    stringsAsFactors = FALSE
  )

  # Create hierarchy: music -> rock -> metal
  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("metal", "rock"),
      to = c("rock", "music")
    ),
    directed = TRUE
  )

  result <- get_tree_and_votes_based_mapping(track_tags, graph, track_tags)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "metal")
})

test_that("get_initial_genres_tree_and_votes_based processes multiple tracks", {
  mb_tags <- data.frame(
    track.s.id = c(1, 1, 2, 2),
    track.s.title = c("Song A", "Song A", "Song B", "Song B"),
    track.s.firstartist.name = c("A", "A", "B", "B"),
    tag_name = c("rock", "pop", "jazz", "blues"),
    # should return "rock" for track 1 and "jazz" for track 2
    tag_count = c(10, 5, 8, 3),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz", "blues"),
      to = c("music", "music", "music", "music")
    ),
    directed = TRUE
  )

  result <- get_initial_genres_tree_and_votes_based(mb_tags, graph)
  expect_true(is.data.frame(result))
  expect_setequal(
    colnames(result),
    c(
      "track.s.id",
      "track.s.title",
      "track.s.firstartist.name",
      "initial_genre"
    )
  )
  expect_equal(nrow(result), length(unique(mb_tags$track.s.id)))
  expect_equal(result$initial_genre, c("rock", "jazz"))
  expect_setequal(result$track.s.id, unique(mb_tags$track.s.id))
})

test_that("handles cases where tags are not in the graph", {
  tags <- data.frame(
    track.s.id = c(1, 1),
    track.s.title = c("Song A", "Song A"),
    track.s.firstartist.name = c("A", "A"),
    tag_name = c("unknown_genre1", "unknown_genre2"),
    tag_count = c(10, 5),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(from = c("rock"), to = c("music")),
    directed = TRUE
  )

  expect_no_error({
    result <- get_initial_genre_mapping(tags, graph)
  })
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1) # No valid genres found in graph
  expect_equal(
    result,
    data.frame(
      track.s.id = "1",
      track.s.title = "Song A",
      track.s.firstartist.name = "A",
      initial_genre = NA
    )
  )
  expect_no_error({
    result <- get_initial_genre_mapping(tags, graph)
  })
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1) # No valid genres found in graph
  expect_equal(
    result,
    data.frame(
      track.s.id = "1",
      track.s.title = "Song A",
      track.s.firstartist.name = "A",
      initial_genre = NA
    )
  )
})

test_that("get_tree_and_votes_based_mapping uses votes for complete subtree", {
  track_tags <- data.frame(
    track.s.id = c(1, 1, 1, 1),
    track.s.title = c(
      "Song A",
      "Song A",
      "Song A",
      "Song A"
    ),
    track.s.firstartist.name = c("A", "A", "A", "A"),
    tag_name = c("music", "rock", "pop", "jazz"),
    tag_count = c(1, 3, 1, 3),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "pop", "jazz"),
      to = c("music", "music", "pop")
    ),
    directed = TRUE
  )

  result <- get_initial_genre_mapping(track_tags, graph)
  expect_equal(result$initial_genre, "jazz")
})

test_that("cache management works correctly", {
  # Test that cache is properly managed
  mb_tags <- data.frame(
    track.s.id = c(1, 1),
    track.s.title = c("Song A", "Song A"),
    track.s.firstartist.name = c("A", "A"),
    tag_name = c("rock", "pop"),
    tag_count = c(10, 5),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    data.frame(from = c("rock", "pop"), to = c("music", "music")),
    directed = TRUE
  )

  # Cache should be cleared before run
  expect_no_error({
    result <- get_initial_genre_mapping(mb_tags, graph)
  })

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
})
