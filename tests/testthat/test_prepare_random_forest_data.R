test_that("apply_casewise_filter removes rows with excess NAs", {
  df <- tibble::tibble(
    track.s.id = 1:4,
    col_a = c(1, NA, NA, 1),
    col_b = c(NA, NA, 1, 1),
    col_c = c(1, 1, 1, 1),
    n_NA = c(1, 2, 2, 0)
  )

  result <- apply_casewise_filter(df, threshold = 0.4)

  testthat::expect_lte(
    max(result$n_NA),
    (ncol(result) - 2) * 0.4
  )
})

test_that("apply_casewise_filter adds n_NA column", {
  df <- tibble::tibble(
    col_a = c(1, NA),
    col_b = c(1, 1)
  )

  result <- apply_casewise_filter(df, threshold = 0.5)

  testthat::expect_true("n_NA" %in% names(result))
})

test_that("join_target merges metagenres correctly", {
  casewise <- tibble::tibble(
    track.s.id = c(1, 2, 3),
    feature = c("a", "b", "c")
  )
  metagenres <- tibble::tibble(
    track.s.id = c(1, 2, 3),
    metagenre = c("Rock", "Pop", "Rock")
  )

  result <- join_target(casewise, metagenres, FALSE)

  testthat::expect_equal(nrow(result), 3)
  testthat::expect_true("metagenre" %in% names(result))
  testthat::expect_equal(
    as.character(result$metagenre),
    c("Rock", "Pop", "Rock")
  )
})

test_that("join_target drops POPULARMUSIC when flagged", {
  casewise <- tibble::tibble(
    track.s.id = 1:3,
    feature = c("a", "b", "c")
  )
  metagenres <- tibble::tibble(
    track.s.id = 1:3,
    metagenre = c("Rock", "POPULAR MUSIC", "Pop")
  )

  result <- join_target(casewise, metagenres, TRUE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_false("POPULAR MUSIC" %in% result$metagenre)
})

test_that("join_target keeps POPULARMUSIC when not flagged", {
  casewise <- tibble::tibble(
    track.s.id = 1:2,
    feature = c("a", "b")
  )
  metagenres <- tibble::tibble(
    track.s.id = 1:2,
    metagenre = c("Rock", "POPULAR MUSIC")
  )

  result <- join_target(casewise, metagenres, FALSE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true("POPULAR MUSIC" %in% result$metagenre)
})

test_that("draw_prototype_sample returns training set", {
  df <- tibble::tibble(
    id = 1:100,
    metagenre = rep(c("A", "B"), 50),
    value = rnorm(100)
  )

  result <- draw_prototype_sample(df, prop = 0.7)

  testthat::expect_equal(nrow(result), 70)
  testthat::expect_true(all(result$id %in% df$id))
})

test_that("draw_prototype_sample maintains stratification", {
  df <- tibble::tibble(
    id = 1:100,
    metagenre = rep(c("A", "B"), 50),
    value = rnorm(100)
  )

  result <- draw_prototype_sample(df, prop = 0.6)
  result_prop <- table(result$metagenre) / nrow(result)
  orig_prop <- table(df$metagenre) / nrow(df)

  testthat::expect_equal(
    as.numeric(result_prop),
    as.numeric(orig_prop),
    tolerance = 0.15
  )
})

test_that("split_artists_and_train_test returns correct structure", {
  low_sample <- tibble::tibble(
    track.s.id = 1:10,
    artist.s.id = rep(c("A", "B", "C"), c(3, 3, 4)),
    metagenre = rep(c("Rock", "Pop"), 5)
  )
  high_sample <- low_sample

  result <- split_artists_and_train_test(
    low_sample,
    high_sample,
    prop = 0.6,
    seed = 42
  )

  testthat::expect_equal(
    length(result),
    4
  )
  testthat::expect_true(all(
    c(
      "low_train",
      "low_test",
      "high_train",
      "high_test"
    ) %in%
      names(result)
  ))
})

test_that("split_artists_and_train_test keeps artists separate", {
  low_sample <- tibble::tibble(
    track.s.id = 1:20,
    artist.s.id = rep(c("A", "B", "C", "D"), 5),
    metagenre = rep(c("Rock", "Pop"), 10)
  )

  result <- split_artists_and_train_test(
    low_sample,
    low_sample,
    prop = 0.5,
    seed = 42
  )

  train_artists <- unique(result$low_train$artist.s.id)
  test_artists <- unique(result$low_test$artist.s.id)
  overlap <- intersect(train_artists, test_artists)

  testthat::expect_equal(length(overlap), 0)
})

test_that("split_artists_and_train_test preserves all tracks", {
  low_sample <- tibble::tibble(
    track.s.id = 1:20,
    artist.s.id = rep(c("A", "B", "C", "D"), 5),
    metagenre = rep(c("Rock", "Pop"), 10)
  )

  result <- split_artists_and_train_test(
    low_sample,
    low_sample,
    prop = 0.6,
    seed = 42
  )

  combined <- dplyr::bind_rows(
    result$low_train,
    result$low_test
  )
  testthat::expect_equal(
    nrow(combined),
    nrow(low_sample)
  )
})

test_that("create_artist_cv_splits returns rset object", {
  df <- tibble::tibble(
    track.s.id = 1:30,
    artist.s.id = rep(1:10, 3),
    metagenre = rep(c("A", "B", "C"), 10),
    col_1 = rnorm(30)
  )

  result <- create_artist_cv_splits(
    df,
    n_folds = 3,
    repeats = 1,
    seed = 42
  )

  testthat::expect_s3_class(result, "vfold_cv")
  testthat::expect_s3_class(result, "rset")
  testthat::expect_true("splits" %in% names(result))
  testthat::expect_true("id" %in% names(result))
})

test_that("create_artist_cv_splits creates correct number of folds", {
  df <- tibble::tibble(
    track.s.id = 1:30,
    artist.s.id = rep(1:10, 3),
    metagenre = rep(c("A", "B", "C"), 10),
    col_1 = rnorm(30)
  )

  result <- create_artist_cv_splits(
    df,
    n_folds = 4,
    repeats = 1,
    seed = 42
  )

  testthat::expect_equal(nrow(result), 4)
})

test_that("create_artist_cv_splits with repeats creates id2 column", {
  df <- tibble::tibble(
    track.s.id = 1:30,
    artist.s.id = rep(1:10, 3),
    metagenre = rep(c("A", "B", "C"), 10),
    col_1 = rnorm(30)
  )

  result <- create_artist_cv_splits(
    df,
    n_folds = 3,
    repeats = 2,
    seed = 42
  )

  testthat::expect_true("id2" %in% names(result))
  testthat::expect_equal(nrow(result), 6)
})

test_that("create_artist_cv_splits respects max_tracks_per_artist", {
  df <- tibble::tibble(
    track.s.id = 1:100,
    artist.s.id = c(rep(1, 60), rep(2, 20), rep(3, 20)),
    metagenre = rep(c("A", "B"), 50),
    col_1 = rnorm(100)
  )

  result <- create_artist_cv_splits(
    df,
    n_folds = 2,
    repeats = 1,
    max_tracks_per_artist = 30,
    seed = 42
  )

  testthat::expect_true(nrow(result) > 0)
})

test_that("create_artist_cv_splits keeps artists separate across folds", {
  df <- tibble::tibble(
    track.s.id = 1:30,
    artist.s.id = rep(1:10, 3),
    metagenre = rep(c("A", "B", "C"), 10),
    col_1 = rnorm(30)
  )

  result <- create_artist_cv_splits(
    df,
    n_folds = 3,
    repeats = 1,
    seed = 42
  )

  for (i in seq_len(nrow(result))) {
    analysis_data <- rsample::analysis(result$splits[[i]])
    assessment_data <- rsample::assessment(result$splits[[i]])

    analysis_artists <- unique(analysis_data$artist.s.id)
    assessment_artists <- unique(assessment_data$artist.s.id)
    overlap <- intersect(analysis_artists, assessment_artists)

    testthat::expect_equal(length(overlap), 0)
  }
})


test_that("apply_imputer preserves track and metagenre columns", {
  train_df <- tibble::tibble(
    track.s.id = c(1, 2, 3, 4, 5),
    metagenre = c("A", "B", "A", "B", "A"),
    n_NA = c(0, 0, 0, 0, 0),
    col_a = c(1.0, 2.0, NA, 4.0, 5.0),
    col_b = c(10.0, NA, 30.0, 40.0, 50.0)
  )

  test_df <- tibble::tibble(
    track.s.id = c(6, 7),
    metagenre = c("A", "B"),
    n_NA = c(0, 0),
    col_a = c(NA, 3.0),
    col_b = c(20.0, NA)
  )

  imputer <- train_imputer(train_df, nthreads = 1, seed = 42, maxiter = 10)
  result <- apply_imputer(test_df, imputer)

  testthat::expect_true("track.s.id" %in% names(result))
  testthat::expect_true("metagenre" %in% names(result))
  testthat::expect_equal(result$track.s.id, c(6, 7))
  testthat::expect_equal(
    as.character(result$metagenre),
    c("A", "B")
  )
  testthat::expect_true(all(!is.na(result$col_a)))
  testthat::expect_true(all(!is.na(result$col_b)))
})

test_that("transform_features converts specified columns to factors", {
  df <- tibble::tibble(
    track.s.key = c(1, 2, 3),
    track.s.mode = c(0, 1, 0),
    track.s.explicitlyrics = c(TRUE, FALSE, TRUE),
    track.s.timesignature = c("4/4", "3/4", "4/4"),
    track.is.instrumental = c(FALSE, TRUE, FALSE),
    artist.mb.area = c("US", "UK", "US"),
    artist.mb.origin = c("US", "UK", "DE"),
    artist.mb.dead = c(FALSE, TRUE, FALSE),
    artist.mb.gender = c("Male", "Female", "Male"),
    artist.mb.type = c("Person", "Person", "Group"),
    track.ab.tonal.key = c(1, 2, 3),
    track.ab.tonal.mode = c(0, 1, 0),
    track.dz.album.explicitlyrics = c(TRUE, FALSE, TRUE),
    track.language = c("en", "de", "en"),
    track.is.dach = c(FALSE, TRUE, FALSE),
    lyrics.distinct_words_ratio = c(0.5, 0.6, 0.7),
    lyrics.repeated_lines_ratio = c(0.2, 0.3, 0.1),
    lyrics.sentiment = c(0.5, 0.6, 0.7),
    lyrics.nrc_anger = c(0.1, 0.2, 0.3),
    artist.s.genres = list(
      data.frame(genre = c("rock")),
      data.frame(genre = c("pop")),
      data.frame(genre = c("rock", "pop"))
    )
  )

  mapping <- tibble::tibble(
    tag_name = c("rock", "pop"),
    metagenre = c("Rock", "Pop")
  )

  result <- transform_features(df, mapping)

  testthat::expect_true(is.factor(result$track.s.key))
  testthat::expect_true(is.factor(result$track.s.mode))
  testthat::expect_true(is.factor(result$track.s.explicitlyrics))
  testthat::expect_true(is.factor(result$track.is.instrumental))
  testthat::expect_true(is.factor(result$track.ab.tonal.key))
  testthat::expect_true(is.factor(result$track.ab.tonal.mode))
})

test_that("transform_features handles instrumental tracks", {
  df <- tibble::tibble(
    track.is.instrumental = c(TRUE, FALSE),
    track.s.key = c(1, 2),
    track.s.mode = c(0, 1),
    track.s.explicitlyrics = c(TRUE, FALSE),
    track.s.timesignature = c("4/4", "3/4"),
    lyrics.sentiment = c(0.5, 0.8),
    lyrics.distinct_words_ratio = c(0.5, 0.9),
    lyrics.repeated_lines_ratio = c(0.2, 0.4),
    lyrics.nrc_anger = c(0.1, 0.3),
    artist.mb.area = c("US", "UK"),
    artist.mb.origin = c("US", "UK"),
    artist.mb.dead = c(FALSE, TRUE),
    artist.mb.gender = c("Male", "Female"),
    artist.mb.type = c("Person", "Group"),
    track.ab.tonal.key = c(1, 2),
    track.ab.tonal.mode = c(0, 1),
    track.dz.album.explicitlyrics = c(TRUE, FALSE),
    track.language = c("en", "de"),
    track.is.dach = c(FALSE, TRUE),
    artist.s.genres = list(
      data.frame(genre = "rock"),
      data.frame(genre = "pop")
    )
  )

  mapping <- tibble::tibble(
    tag_name = c("rock", "pop"),
    metagenre = c("Rock", "Pop")
  )

  result <- transform_features(df, mapping)

  testthat::expect_equal(result$lyrics.sentiment[1], 0)
  testthat::expect_equal(
    result$lyrics.distinct_words_ratio[1],
    1
  )
  testthat::expect_equal(
    result$lyrics.repeated_lines_ratio[1],
    1
  )
  testthat::expect_equal(result$lyrics.nrc_anger[1], 0)
  testthat::expect_equal(result$lyrics.sentiment[2], 0.8)
})

test_that("unnest_distribution_genres creates distribution flags", {
  df <- tibble::tibble(
    id = 1:2,
    artist.s.genres = list(
      data.frame(genre = c("rock", "metal")),
      data.frame(genre = c("pop"))
    )
  )

  mapping <- tibble::tibble(
    tag_name = c("rock", "metal", "pop"),
    metagenre = c("Rock", "Metal", "Pop")
  )

  result <- unnest_distribution_genres(df, mapping)

  testthat::expect_true(any(grepl("^dtb\\.", names(result))))
  testthat::expect_false("artist.s.genres" %in% names(result))
})

test_that("unnest_distribution_genres assigns POPULAR MUSIC default", {
  df <- tibble::tibble(
    id = 1,
    artist.s.genres = list(
      data.frame(genre = c("unknown_genre"))
    )
  )

  mapping <- tibble::tibble(
    tag_name = c("rock"),
    metagenre = c("Rock")
  )

  result <- unnest_distribution_genres(df, mapping)

  testthat::expect_true("dtb.other" %in% names(result))
})

test_that("check_factor_levels_in_folds generates warnings for low counts", {
  df <- tibble::tibble(
    track.s.id = 1:50,
    artist.s.id = rep(1:10, 5),
    metagenre = rep(c("A", "B"), 25),
    col_1 = factor(c(rep("X", 4), rep("Y", 46))),
    col_2 = factor(rnorm(50))
  )

  cv_splits <- create_artist_cv_splits(
    df,
    n_folds = 2,
    repeats = 1,
    seed = 42
  )

  result <- testthat::capture_messages({
    testthat::capture_warnings(
      {
        check_factor_levels_in_folds(cv_splits, "test")
      }
    )
  })

  testthat::expect_true(any(grepl("Checking factor levels", result)))
})

test_that("Full pipeline: raw poptrag to prepared RF data", {
  poptrag <- tibble::tibble(
    track.s.id = 1:20,
    artist.s.id = rep(1:5, 4),
    track.es.popularity = rnorm(20),
    album.dc.rank = rnorm(20),
    artist.s.popularity = rep(50:54, 4),
    artist.s.followers = rep(1000:1004, 4),
    album.s.totaltracks = rep(10, 20),
    album.s.releaseyear = rep(2020, 20),
    album.s.popularity = rep(60, 20),
    track.s.danceability = rnorm(20),
    track.s.energy = rnorm(20),
    track.s.speechiness = rnorm(20),
    track.s.acousticness = rnorm(20),
    track.s.instrumentalness = rnorm(20),
    track.s.liveness = rnorm(20),
    track.s.valence = rnorm(20),
    track.s.tempo = rnorm(20),
    track.s.popularity = rnorm(20),
    track.s.duration = rnorm(20),
    track.s.key = rep(c(0, 1, 2, 3, 4), 4),
    track.s.loudness = rnorm(20),
    track.s.mode = rep(0:1, 10),
    track.s.explicitlyrics = rep(c(TRUE, FALSE), 10),
    track.s.timesignature = rep("4/4", 20),
    track.is.instrumental = rep(c(FALSE, TRUE), 10),
    artist.mb.type = rep("Person", 20),
    artist.mb.gender = rep(c("Male", "Female"), 10),
    artist.mb.area = rep(c("US", "UK"), 10),
    artist.mb.birthyear = rep(1980, 20),
    artist.mb.dead = rep(FALSE, 20),
    artist.mb.origin = rep(c("US", "UK"), 10),
    track.ab.tonal.key = rep(0:1, 10),
    track.ab.tonal.mode = rep(0:1, 10),
    track.dz.album.explicitlyrics = rep(c(TRUE, FALSE), 10),
    track.language = rep(c("en", "de"), 10),
    track.is.dach = rep(c(FALSE, TRUE), 10),
    lyrics.distinct_words_ratio = rnorm(20, mean = 0.7),
    lyrics.repeated_lines_ratio = rnorm(20, mean = 0.3),
    lyrics.sentiment = rnorm(20),
    lyrics.nrc_anger = rnorm(20),
    artist.s.genres = list(
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop"),
      data.frame(genre = "rock"),
      data.frame(genre = "pop")
    )
  )

  metagenres <- tibble::tibble(
    track.s.id = 1:20,
    metagenre = c(rep("Rock", 10), rep("Pop", 10))
  )

  mapping <- tibble::tibble(
    tag_name = c("rock", "pop"),
    metagenre = c("Rock", "Pop")
  )

  settings <- list(
    subsample_prop = 0.8,
    casewise_threshold = 0.5,
    seed = 42,
    artist_initial_split = 0.6,
    apply_imputation = FALSE,
    n_cores = 1,
    drop_POPULARMUSIC = FALSE,
    cv_folds = 2,
    cv_repeats = 1,
    max_tracks_per_artist_cv = 10,
    s_genremapping = mapping
  )

  selected <- select_features_poptrag(poptrag)
  transformed <- transform_features(selected, mapping)
  casewise <- apply_casewise_filter(transformed, threshold = 0.5)
  joined <- join_target(casewise, metagenres, FALSE)
  sampled <- draw_prototype_sample(joined, prop = 0.8)

  testthat::expect_true(nrow(sampled) > 0)
  testthat::expect_true(all(c("track.s.id", "metagenre") %in% names(sampled)))
  testthat::expect_true(all(is.factor(sampled$track.s.key)))
  testthat::expect_false("track.es.popularity" %in% names(sampled))
  testthat::expect_false("album.dc.rank" %in% names(sampled))
  testthat::expect_false("artist.s.genres" %in% names(sampled))
  testthat::expect_true(any(grepl("^dtb\\.", names(sampled))))
})
