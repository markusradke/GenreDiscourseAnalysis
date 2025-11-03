test_that("map_metagenres maps genres correctly", {
  long <- data.frame(
    track.s.id = c("1", "1", "2", "3", "3", "3"),
    tag_name = c(
      "alternative rock",
      "classical",
      "classical",
      "jazz",
      "classical",
      "rock"
    ),
    tag_count = c(10, 5, 8, 12, 3, 16)
  )
  metagenre_mapping <- data.frame(
    tag_name = c(
      "alternative rock",
      "rock",
      "classical",
      "jazz"
    ),
    metagenre = c(
      "rock",
      "rock",
      "classical",
      "jazz"
    )
  )
  expect_no_error({
    mapped <- map_metagenres(long, metagenre_mapping)
  })
  expect_equal(nrow(mapped), 3)
  expect_equal(mapped$metagenre[mapped$track.s.id == "1"], "rock")
  expect_equal(mapped$metagenre[mapped$track.s.id == "2"], "classical")
  expect_equal(mapped$metagenre[mapped$track.s.id == "3"], "rock")
  # check that result also contains the probabilities for all metagenres
  expect_setequal(
    colnames(mapped),
    c("track.s.id", "p_rock", "p_classical", "p_jazz", "metagenre")
  )
})

test_that("get_metagenre_prevalences calculates prevalences correctly", {
  long_with_metagenres <- data.frame(
    track.s.id = c("1", "1", "2", "3", "3", "3"),
    metagenre = c(
      "rock",
      "classical",
      "classical",
      "jazz",
      "classical",
      "rock"
    ),
    tag_count = c(10, 5, 8, 12, 3, 16)
  )
  expect_no_error({
    prevalences <- get_metagenre_prevalences(long_with_metagenres)
  })
  expect_equal(nrow(prevalences), 6)
  expect_equal(
    prevalences$metagenre_prevalence[
      prevalences$track.s.id == "1" & prevalences$metagenre == "rock"
    ],
    10 / (10 + 5)
  )
  expect_equal(
    prevalences$metagenre_prevalence[
      prevalences$track.s.id == "2" & prevalences$metagenre == "classical"
    ],
    1
  )
  expect_equal(
    prevalences$metagenre_prevalence[
      prevalences$track.s.id == "3" & prevalences$metagenre == "rock"
    ],
    16 / (12 + 3 + 16)
  )
})

test_that("get_metagenre_probs_df returns correct format", {
  metagenre_prevalences <- data.frame(
    track.s.id = c("1", "1", "2", "3", "3", "3"),
    metagenre = c(
      "rock",
      "classical music",
      "classical music",
      "jazz",
      "classical music",
      "rock"
    ),
    metagenre_prevalence = c(0.67, 0.33, 1.0, 0.4, 0.1, 0.5)
  )
  all_metagenres <- c("rock", "classical music", "jazz", "pop")
  expect_no_error({
    probs_df <- get_metagenre_probs_df(metagenre_prevalences, all_metagenres)
  })
  expect_setequal(
    colnames(probs_df),
    c(
      "track.s.id",
      gsub(" ", "", paste("p_", all_metagenres, sep = "")),
      "metagenre"
    )
  )
  expect_equal(nrow(probs_df), 3)
  expect_equal(probs_df$p_pop, c(0, 0, 0))
  expect_equal(probs_df$p_rock[probs_df$track.s.id == "1"], 0.67)
  expect_equal(
    rowSums(probs_df[, grep("^p_", colnames(probs_df))]),
    c(1, 1, 1)
  )
  expect_equal(
    probs_df$metagenre,
    c("rock", "classical music", "rock")
  )
})

test_that("add_unmentioned_cols_zero adds missing columns with zeros", {
  df <- data.frame(
    track.s.id = c("1", "2"),
    p_rock = c(0.7, 0.4),
    p_jazz = c(0.3, 0.6)
  )
  expected_cols <- c("p_rock", "p_jazz", "p_classical", "p_pop")
  result <- add_unmentioned_cols_zero(df, expected_cols)
  expect_setequal(colnames(result), c(expected_cols, "track.s.id"))
  expect_equal(result$p_classical, c(0, 0))
  expect_equal(result$p_pop, c(0, 0))
})

test_that("get_hard_classes uses fewer weighted sum of tag_counts to break ties", {
  metagenre_probs_df <- data.frame(
    track.s.id = c("1", "2"),
    p_rock = c(0.5, 0),
    p_jazz = c(0.5, 0.5)
  )
  metagenre_prevalences <- data.frame(
    track.s.id = c("1", "1", "2"),
    metagenre = c("rock", "jazz", "rock"),
    metagenre_prevalence = c(0.5, 0.5, 1),
    tag_count = c(20, 20, 10) # jazz has lower overall tag_count sum
  )

  out <- get_hard_classes(metagenre_probs_df, metagenre_prevalences)
  expect_equal(out$metagenre[out$track.s.id == "1"], "jazz")
})
