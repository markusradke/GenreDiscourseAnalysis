test_that("choose_mb_tag_set prefers track over album over artist", {
  track <- data.frame(tag_name = c("a"), tag_count = 1)
  album <- data.frame(tag_name = c("b"), tag_count = 2)
  artist <- data.frame(tag_name = c("c"), tag_count = 3)

  empty <- data.frame()
  expect_equal(choose_mb_tag_set(track, album, artist), track)
  expect_equal(choose_mb_tag_set(empty, album, artist), album)
  expect_equal(choose_mb_tag_set(empty, empty, artist), artist)
})

test_that("get_combined_dc_tags and normalize_tags handle NA and NULL", {
  df <- data.frame(
    album.dc.genres = I(list(NA, c("Rock"))),
    album.dc.styles = I(list(NULL, c("Indie")))
  )
  out <- get_combined_dc_tags(df)
  expect_true("dc.genres" %in% colnames(out))
  expect_true(all(sapply(out$dc.genres, is.data.frame)))
})


test_that("erase_non_music_tags removes non-music tags", {
  tags <- list(
    data.frame(tag_name = c("rock", "non-music"), tag_count = c(10, 1)),
    data.frame(tag_name = c("pop"), tag_count = 5)
  )
  non_music <- c("non-music", "interview")
  out <- erase_non_music_tags(tags, non_music)
  expect_true(all(sapply(out, function(df) !any(df$tag_name %in% non_music))))
  expect_equal(out[[2]]$tag_name, "pop")
})

test_that("filter_non_empty_tags filters by provided genre column", {
  df <- data.frame(track.s.id = 1:3, stringsAsFactors = FALSE)
  df$genres <- list(
    data.frame(tag_name = "a", tag_count = 1),
    data.frame(),
    NULL
  )
  res <- filter_non_empty_tags(df, "genres")
  expect_equal(nrow(res), 1)
  expect_equal(res$track.s.id, 1)
})

test_that("unpack_genre_tags and get_long_genre_tags work together", {
  tags <- list(
    data.frame(tag_name = c("a", "b"), tag_count = c(1, 2)),
    data.frame(tag_name = "c", tag_count = 3)
  )
  unpacked <- unpack_genre_tags(tags)
  expect_equal(nrow(unpacked), 3)
  expect_true(all(
    c("join_id", "tag_name", "tag_count") %in% colnames(unpacked)
  ))

  input <- data.frame(
    track.s.id = c(1, 2),
    track.s.title = c("t1", "t2"),
    track.s.firstartist.name = c("a1", "a2"),
    stringsAsFactors = FALSE
  )
  input$genres <- tags
  out <- get_long_genre_tags(input, "genres")
  expect_true(all(c("track.s.id", "tag_name", "tag_count") %in% colnames(out)))
  expect_equal(nrow(out), 3)
})

test_that("filter_music_tags removes rows with only non-music tags", {
  df <- data.frame(track.s.id = c(1, 2), stringsAsFactors = FALSE)
  df$genres <- list(
    data.frame(tag_name = c("non-music"), tag_count = 1),
    data.frame(tag_name = c("rock"), tag_count = 2)
  )
  non_music <- c("non-music")
  res <- filter_music_tags(df, "genres", non_music)
  expect_equal(nrow(res), 1)
  expect_equal(res$track.s.id, 2)
})

test_that("filter_valid_mb_genres composes the correct pipeline", {
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
  expect_true(all(sapply(out$mb.genres, function(x) {
    is.data.frame(x) && nrow(x) > 0 && ncol(x) == 2
  })))
})

test_that("filter_valid_dc_genres composes the correct pipeline", {
  df <- data.frame(
    track.s.id = c("a", "b", "c"),
    album.dc.genres = I(list(NA, c("Rock"), c("Children's"))),
    album.dc.styles = I(list(NULL, c("Indie"), NA))
  )

  non_music <- c("Children's")
  out <- filter_valid_dc_genres(df, non_music)
  expect_equal(nrow(out), 1)
  expect_true(all(sapply(out$dc.genres, function(x) {
    is.data.frame(x) && nrow(x) > 0 && ncol(x) == 2
  })))
})
