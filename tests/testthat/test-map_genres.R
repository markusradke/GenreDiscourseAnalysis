test_that("get_initial_genre_mapping dispatches correctly by platform", {
  tags <- data.frame(
    track.s.id = c(1, 1, 2, 2),
    track.s.title = c("Song A", "Song A", "Song B", "Song B"),
    track.s.firstartist.name = c("A", "A", "B", "B"),
    tag_name = c("rock", "music", "music", "rock"),
    tag_count = c(10, 5, 8, 3),
    stringsAsFactors = FALSE
  )
  graph <- igraph::graph_from_data_frame(
    data.frame(from = c("rock"), to = c("music")),
    directed = TRUE
  )

  expect_no_error({
    result_mb <- get_initial_genre_mapping(tags, graph, "MusicBrainz")
  })

  expect_no_error({
    result_dc <- get_initial_genre_mapping(tags, graph, "Discogs")
  })

  expect_no_error({
    result_sp <- get_initial_genre_mapping(tags, graph, "Spotify")
  })

  expect_error(
    get_initial_genre_mapping(tags, graph, "Something else"),
    "Unknown platform"
  )
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

test_that("get_initial_genres_most_detailed selects deepest genres with least votes", {
  tags <- data.frame(
    track.s.id = c(1, 1, 2, 2, 2),
    track.s.title = c("Song A", "Song A", "Song B", "Song B", "Song B"),
    track.s.firstartist.name = c("A", "A", "B", "B", "B"),
    tag_name = c("metal", "rock", "pop", "rock", "music"),
    tag_count = c(1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Create hierarchy: music (level 0) -> rock (level 1) -> metal (level 2)
  #                   music (level 0) -> pop (level 1)
  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("metal", "rock", "pop"),
      to = c("rock", "music", "music")
    ),
    directed = TRUE
  )

  result <- get_initial_genres_most_detailed(tags, graph)
  expect_true(is.data.frame(result))
  expect_setequal(
    colnames(result),
    c(
      "track.s.id",
      "initial_genre",
      "track.s.title",
      "track.s.firstartist.name"
    )
  )
  # Track 1 should prefer metal (deeper level) over rock
  track1_genre <- result[result$track.s.id == 1, "initial_genre", drop = TRUE]
  expect_equal(track1_genre, "metal")
  # Track 2 should prefer poc (fewer votes) when levels are equal
  track2_genre <- result[result$track.s.id == 2, "initial_genre", drop = TRUE]
  expect_equal(track2_genre, "pop")
})

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

  result <- get_tree_and_votes_based_mapping(track_tags, graph)
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

test_that("functions handle empty input gracefully", {
  empty_tags <- data.frame(
    track.s.id = numeric(0),
    track.s.title = character(0),
    track.s.firstartist.name = character(0),
    tag_name = character(0),
    tag_count = numeric(0),
    stringsAsFactors = FALSE
  )

  graph <- igraph::make_empty_graph(n = 1, directed = TRUE)
  igraph::V(graph)$name <- "music"
  expect_no_error({
    result <- get_initial_genres_most_detailed(empty_tags, graph)
  })
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true(all(c("track.s.id", "initial_genre") %in% colnames(result)))
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
    result <- get_initial_genre_mapping(tags, graph, "MusicBrainz")
  })
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1) # No valid genres found in graph
  expect_equal(
    result,
    data.frame(
      track.s.id = 1,
      track.s.title = "Song A",
      track.s.firstartist.name = "A",
      initial_genre = NA
    )
  )
  expect_no_error({
    result <- get_initial_genre_mapping(tags, graph, "Spotify")
  })
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1) # No valid genres found in graph
  expect_equal(
    result,
    data.frame(
      track.s.id = 1,
      track.s.title = "Song A",
      track.s.firstartist.name = "A",
      initial_genre = NA
    )
  )
})
