test_that("choose_mb_tag_set prefers track over album over artist", {
  track <- data.frame(tag_name = c("a"), tag_count = 1)
  album <- data.frame(tag_name = c("b"), tag_count = 2)
  artist <- data.frame(tag_name = c("c"), tag_count = 3)

  empty <- data.frame()
  expect_equal(choose_mb_tag_set(track, album, artist), track)
  expect_equal(choose_mb_tag_set(empty, album, artist), album)
  expect_equal(choose_mb_tag_set(empty, empty, artist), artist)
})

test_that("erase_non_music_tags removes non-music tags", {
  tags <- list(
    data.frame(tag_name = c("rock", "non-music"), tag_count = c(10, 1)),
    data.frame(tag_name = c("pop"), tag_count = 5)
  )
  non_music <- c("non-music", "interview")
  out <- erase_non_music_tags(tags, non_music)
  expect_true(all(sapply(out, function(df) !any(df$tag_name %in% non_music))))
  # ensure other tags preserved
  expect_equal(out[[2]]$tag_name, "pop")
})

test_that("filter_non_empty_tags keeps rows with mb.genres", {
  df <- data.frame(track.s.id = c("a", "b", "c"))
  df$mb.genres <- list(
    data.frame(tag_name = "a", tag_count = 1),
    data.frame(),
    NULL
  )

  res <- filter_non_empty_tags(df)
  expect_equal(nrow(res), 1)
  expect_equal(res$track.s.id, "a")
})

test_that("unpack_genre_tags and unpack_mb_genre_tags work together", {
  tags <- list(
    data.frame(tag_name = c("a", "b"), tag_count = c(1, 2)),
    data.frame(tag_name = "c", tag_count = 3)
  )
  unpacked <- unpack_genre_tags(tags)
  # expect 3 rows and join_id present
  expect_equal(nrow(unpacked), 3)
  expect_true(all(
    c("join_id", "tag_name", "tag_count") %in% colnames(unpacked)
  ))

  # test unpack_mb_genre_tags integrates with input frame
  input <- data.frame(
    track.s.id = c("a", "b"),
    track.s.title = c("t1", "t2"),
    track.s.firstartist.name = c("a1", "a2"),
    stringsAsFactors = FALSE
  )
  input$mb.genres <- tags
  out <- unpack_mb_genre_tags(input)
  expect_true(all(
    c("track.s.id", "tag_name", "tag_count") %in% colnames(out)
  ))
  expect_equal(nrow(out), 3)
})

test_that("filter_music_tags removes entries with only non-music tags", {
  df <- data.frame(track.s.id = c("a", "b"), stringsAsFactors = FALSE)
  df$mb.genres <- list(
    data.frame(tag_name = c("non-music"), tag_count = 1),
    data.frame(tag_name = c("rock"), tag_count = 2)
  )
  non_music <- c("non-music")
  res <- filter_music_tags(df, non_music)
  expect_equal(nrow(res), 1)
  expect_equal(res$track.s.id, "b")
})

test_that("filter_valid_mb_genres composes the pipeline", {
  # create input with track, album, artist tag frames
  df <- data.frame(track.s.id = 1:2, stringsAsFactors = FALSE)
  df$track.mb.genres <- list(
    data.frame(tag_name = c("a"), tag_count = 1),
    data.frame()
  )
  df$album.mb.genres <- list(
    data.frame(),
    data.frame(tag_name = c("b"), tag_count = 2)
  )
  df$artist.mb.genres <- list(
    data.frame(),
    data.frame()
  )

  non_music <- c("non-music")
  out <- filter_valid_mb_genres(df, non_music)
  expect_equal(nrow(out), 2)
  # both rows should have mb.genres non-empty and be data.frames
  expect_true(all(sapply(out$mb.genres, function(x) {
    is.data.frame(x) && nrow(x) > 0
  })))
})
