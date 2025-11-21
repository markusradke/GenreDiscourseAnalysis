test_that("undersampling respects max_tracks_per_artist", {
  artists <- rep(paste0("a", 1:4), times = c(1, 3, 5, 2))
  met <- rep(c("m1", "m2", "m1", "m2"), times = c(1, 3, 5, 2))

  tr <- data.frame(
    artist.s.id = artists,
    metagenre = met,
    stringsAsFactors = TRUE
  )

  max_t <- 2

  cv <- create_artist_cv_splits(
    tr,
    n_folds = 2,
    repeats = 1,
    max_tracks_per_artist = max_t,
    seed = 42
  )

  split_data <- cv$splits[[1]]$data

  counts <- table(tr$artist.s.id)
  expected_n <- sum(pmin(as.integer(counts), max_t))

  expect_equal(nrow(split_data), expected_n)
})

test_that("returned object has vfold_cv class and attributes", {
  tr <- data.frame(
    artist.s.id = paste0("a", 1:6),
    metagenre = rep(c("x", "y"), 3),
    stringsAsFactors = TRUE
  )

  cv <- create_artist_cv_splits(
    tr,
    n_folds = 3,
    repeats = 2,
    max_tracks_per_artist = 10,
    seed = 1
  )

  expect_true("vfold_cv" %in% class(cv))
  expect_equal(attr(cv, "v"), 3)
  expect_equal(attr(cv, "repeats"), 2)
  expect_true("id2" %in% names(cv))
  expect_equal(nrow(cv), length(cv$splits))
})

test_that("no artist appears in multiple assessment folds", {
  tr <- data.frame(
    artist.s.id = rep(paste0("art", 1:8), each = 2),
    metagenre = rep(c("g1", "g2"), 8),
    stringsAsFactors = TRUE
  )

  cv <- create_artist_cv_splits(
    tr,
    n_folds = 4,
    repeats = 1,
    max_tracks_per_artist = 5,
    seed = 2
  )

  assessment_artists <- unlist(lapply(seq_len(nrow(cv)), function(i) {
    rsample::assessment(cv$splits[[i]])$artist.s.id |>
      unique()
  }))
  tbl <- table(assessment_artists)

  expect_true(all(as.integer(tbl) == 1))
})
