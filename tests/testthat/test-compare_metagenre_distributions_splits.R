test_that("build_split_df creates correct structure", {
  counts <- c("rock" = 10, "pop" = 5, "jazz" = 3)
  result <- build_split_df(counts, "Train", "split_1")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 5)
  expect_true(all(
    c(
      "metagenre",
      "count",
      "proportion",
      "split_type",
      "split_id"
    ) %in%
      colnames(result)
  ))
})

test_that("build_split_df computes proportions correctly", {
  counts <- c("rock" = 20, "pop" = 30)
  result <- build_split_df(counts, "Train", "split_1")

  expect_equal(result$count, c(20L, 30L))
  expect_equal(result$proportion, c(0.4, 0.6))
})

test_that("build_split_df preserves metagenre names from table", {
  counts <- table(c("metal", "metal", "indie", "indie", "indie"))
  result <- build_split_df(counts, "Test", "split_2")

  expect_setequal(result$metagenre, c("indie", "metal"))
})

test_that("build_split_df sets split_type and split_id correctly", {
  counts <- c("rock" = 5)
  result <- build_split_df(counts, "CV_Analysis", "fold_1")

  expect_equal(result$split_type, "CV_Analysis")
  expect_equal(result$split_id, "fold_1")
})

test_that("extract_cv_dfs returns empty list for NULL input", {
  result <- extract_cv_dfs(NULL)

  expect_type(result, "list")
  expect_length(result, 0)
})


test_that("extract_cv_dfs creates correct id with single id column", {
  data <- data.frame(
    metagenre = c("rock", "rock", "pop", "pop", "jazz", "jazz")
  )
  cv_splits <- rsample::vfold_cv(data, v = 2)

  result <- extract_cv_dfs(cv_splits)

  expect_true(all(
    sapply(result, function(x) {
      all(grepl("Fold", x$split_id[1]))
    })
  ))
})

test_that("extract_cv_dfs creates correct id with id and id2 columns", {
  data <- data.frame(
    metagenre = rep(c("rock", "pop", "jazz"), 2)
  )
  cv_splits <- rsample::vfold_cv(data, v = 2, repeats = 2)

  result <- extract_cv_dfs(cv_splits)

  expect_true(all(
    sapply(result, function(x) {
      all(grepl("Repeat[1-9]_Fold", x$split_id[1]))
    })
  ))
})

test_that("extract_cv_dfs extracts analysis and assessment data", {
  data <- data.frame(
    metagenre = c("rock", "rock", "pop", "pop", "jazz", "jazz")
  )
  cv_splits <- rsample::vfold_cv(data, v = 2)

  result <- extract_cv_dfs(cv_splits)

  split_types <- unique(sapply(result, function(x) unique(x$split_type)))
  expect_setequal(
    split_types,
    c("CV_Analysis", "CV_Assessment")
  )
})

test_that("extract_cv_dfs computes proportions correctly", {
  data <- data.frame(
    metagenre = c(
      "rock",
      "rock",
      "rock",
      "rock",
      "pop",
      "pop",
      "jazz"
    )
  )
  cv_splits <- rsample::vfold_cv(data, v = 2)

  result <- extract_cv_dfs(cv_splits)

  for (df in result) {
    expect_true(all(df$proportion >= 0 & df$proportion <= 1))
    expect_equal(sum(df$proportion), 1, tolerance = 1e-10)
  }
})

test_that("compute_summary_stats returns data frame with correct columns", {
  all_data <- data.frame(
    metagenre = c("rock", "rock", "pop", "jazz"),
    count = c(10, 15, 20, 5),
    proportion = c(0.25, 0.375, 0.5, 0.125),
    split_type = c("Train", "Train", "Test", "Test"),
    split_id = c("s1", "s1", "s2", "s2")
  )

  result <- compute_summary_stats(all_data)

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c(
      "metagenre",
      "split_type",
      "mean_prop",
      "sd_prop",
      "min_prop",
      "max_prop",
      "n_splits"
    ) %in%
      colnames(result)
  ))
})

test_that("compute_summary_stats computes statistics correctly", {
  all_data <- data.frame(
    metagenre = c("rock", "rock", "rock", "pop", "pop"),
    count = c(10, 20, 15, 30, 40),
    proportion = c(0.2, 0.4, 0.3, 0.6, 0.8),
    split_type = c("Train", "Train", "Train", "Test", "Test"),
    split_id = c("s1", "s2", "s3", "s1", "s2")
  )

  result <- compute_summary_stats(all_data)

  rock_row <- result[result$metagenre == "rock", ]
  expect_equal(rock_row$mean_prop, mean(c(0.2, 0.4, 0.3)))
  expect_equal(rock_row$n_splits, 3)
})

test_that("build_all_data combines train, test, and cv splits", {
  train <- data.frame(
    metagenre = c("rock", "rock", "pop")
  )
  test <- data.frame(
    metagenre = c("rock", "jazz")
  )
  cv_splits <- rsample::vfold_cv(
    data.frame(metagenre = c("rock", "pop", "jazz")),
    v = 2
  )

  dataset <- list(
    train = train,
    test = test,
    cv_splits = cv_splits
  )
  result <- build_all_data(dataset)

  split_types <- unique(result$split_type)
  expect_true("Train" %in% split_types)
  expect_true("Test" %in% split_types)
  expect_true("CV_Analysis" %in% split_types)
  expect_true("CV_Assessment" %in% split_types)
})

test_that("build_all_data counts metagenres from train correctly", {
  train <- data.frame(
    metagenre = c("rock", "rock", "rock", "pop", "pop")
  )
  test <- data.frame(metagenre = c("rock"))
  cv_splits <- NULL

  dataset <- list(train = train, test = test, cv_splits = cv_splits)
  result <- build_all_data(dataset)

  train_data <- result[result$split_type == "Train", ]
  rock_count <- train_data[train_data$metagenre == "rock", "count"]
  pop_count <- train_data[train_data$metagenre == "pop", "count"]

  expect_equal(rock_count, 3)
  expect_equal(pop_count, 2)
})

test_that("build_all_data counts metagenres from test correctly", {
  train <- data.frame(metagenre = c("rock"))
  test <- data.frame(
    metagenre = c("rock", "rock", "pop", "pop", "jazz")
  )
  cv_splits <- NULL

  dataset <- list(train = train, test = test, cv_splits = cv_splits)
  result <- build_all_data(dataset)

  test_data <- result[result$split_type == "Test", ]
  rock_count <- test_data[test_data$metagenre == "rock", "count"]
  pop_count <- test_data[test_data$metagenre == "pop", "count"]
  jazz_count <- test_data[test_data$metagenre == "jazz", "count"]

  expect_equal(rock_count, 2)
  expect_equal(pop_count, 2)
  expect_equal(jazz_count, 1)
})

test_that("make_plot returns ggplot object", {
  all_data <- data.frame(
    metagenre = c("rock", "rock", "pop"),
    proportion = c(0.5, 0.4, 0.6),
    split_type = c("Train", "Test", "Train"),
    split_id = c("s1", "s1", "s2")
  )

  result <- make_plot(all_data)

  expect_s3_class(result, "ggplot")
})

test_that("make_plot includes correct layers", {
  all_data <- data.frame(
    metagenre = c("rock", "rock", "pop", "pop"),
    proportion = c(0.5, 0.4, 0.6, 0.3),
    split_type = c("Train", "Test", "CV_Analysis", "CV_Assessment"),
    split_id = c("s1", "s1", "cv1", "cv1")
  )

  result <- make_plot(all_data)

  layers <- sapply(result$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% layers)
})


test_that("compare_metagenre_distributions_splits returns list", {
  train <- data.frame(
    metagenre = c("rock", "rock", "pop")
  )
  test <- data.frame(
    metagenre = c("rock", "jazz")
  )
  cv_splits <- rsample::vfold_cv(
    data.frame(metagenre = c("rock", "pop", "jazz")),
    v = 2
  )

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("proportions" %in% names(result))
})

test_that("compare_metagenre_distributions_splits returns plot", {
  train <- data.frame(
    metagenre = c("rock", "rock", "pop")
  )
  test <- data.frame(
    metagenre = c("rock", "jazz")
  )
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)

  expect_s3_class(result$plot, "ggplot")
})

test_that("compare_metagenre_distributions_splits returns proportions", {
  train <- data.frame(
    metagenre = c("rock", "rock", "pop")
  )
  test <- data.frame(
    metagenre = c("rock", "jazz")
  )
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)

  expect_s3_class(result$proportions, "data.frame")
  expect_true("metagenre" %in% colnames(result$proportions))
  expect_true("proportion" %in% colnames(result$proportions))
})

test_that("compare_metagenre_distributions_splits includes train split", {
  train <- data.frame(
    metagenre = c("rock", "rock", "pop")
  )
  test <- data.frame(
    metagenre = c("rock")
  )
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)
  proportions <- result$proportions

  expect_true("Train" %in% proportions$split_type)
})

test_that("compare_metagenre_distributions_splits includes test split", {
  train <- data.frame(
    metagenre = c("rock")
  )
  test <- data.frame(
    metagenre = c("rock", "rock", "pop")
  )
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)
  proportions <- result$proportions

  expect_true("Test" %in% proportions$split_type)
})

test_that("compare_metagenre_distributions_splits includes cv splits", {
  train <- data.frame(
    metagenre = c("rock", "pop")
  )
  test <- data.frame(
    metagenre = c("rock")
  )
  cv_splits <- rsample::vfold_cv(
    data.frame(metagenre = c("rock", "rock", "pop", "pop")),
    v = 2
  )

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)
  proportions <- result$proportions

  expect_true(
    "CV_Analysis" %in%
      proportions$split_type ||
      "CV_Assessment" %in% proportions$split_type
  )
})

test_that("compare_metagenre_distributions_splits computes correct counts", {
  train <- data.frame(
    metagenre = c("rock", "rock", "rock", "pop", "pop")
  )
  test <- data.frame(
    metagenre = c("rock", "jazz")
  )
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)
  proportions <- result$proportions

  train_rock <- proportions[
    proportions$split_type == "Train" &
      proportions$metagenre == "rock",
    "count"
  ]
  train_pop <- proportions[
    proportions$split_type == "Train" &
      proportions$metagenre == "pop",
    "count"
  ]
  test_rock <- proportions[
    proportions$split_type == "Test" &
      proportions$metagenre == "rock",
    "count"
  ]

  expect_equal(train_rock, 3)
  expect_equal(train_pop, 2)
  expect_equal(test_rock, 1)
})

test_that("compare_metagenre_distributions_splits handles NULL cv_splits", {
  train <- data.frame(metagenre = c("rock", "pop"))
  test <- data.frame(metagenre = c("jazz"))
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)

  expect_s3_class(result$plot, "ggplot")
  expect_type(result$proportions, "list")
})

test_that("extract_cv_dfs maintains correct split ids across folds", {
  data <- data.frame(
    metagenre = rep(c("rock", "pop", "jazz"), 4)
  )
  cv_splits <- rsample::vfold_cv(data, v = 3)

  result <- extract_cv_dfs(cv_splits)

  split_ids <- unique(sapply(result, function(x) unique(x$split_id)))
  expect_length(split_ids, 3)
})

test_that("compute_summary_stats groups by both metagenre and split_type", {
  all_data <- data.frame(
    metagenre = c("rock", "rock", "rock", "rock", "pop", "pop"),
    proportion = c(0.5, 0.4, 0.3, 0.6, 0.2, 0.1),
    split_type = c("Train", "Train", "Train", "Test", "Train", "Test"),
    split_id = c("s1", "s2", "s3", "s1", "s2", "s2")
  )

  result <- compute_summary_stats(all_data)

  rock_train <- result[
    result$metagenre == "rock" & result$split_type == "Train",
  ]
  rock_test <- result[
    result$metagenre == "rock" & result$split_type == "Test",
  ]

  expect_equal(rock_train$n_splits, 3)
  expect_equal(rock_test$n_splits, 1)
})

test_that("make_plot handles multiple metagenres", {
  all_data <- data.frame(
    metagenre = c(
      "rock",
      "rock",
      "pop",
      "pop",
      "jazz",
      "jazz",
      "metal",
      "metal",
      "indie",
      "indie"
    ),
    proportion = c(0.2, 0.3, 0.15, 0.2, 0.25, 0.22, 0.18, 0.19, 0.2, 0.16),
    split_type = c(
      "Train",
      "Test",
      "Train",
      "Test",
      "Train",
      "Test",
      "Train",
      "Test",
      "Train",
      "Test"
    ),
    split_id = c("s1", "s1", "s2", "s2", "s3", "s3", "s4", "s4", "s5", "s5")
  )

  result <- make_plot(all_data)

  expect_s3_class(result, "ggplot")
})

test_that("Integration: compare_metagenre_distributions_splits with cv_splits", {
  set.seed(42)
  train <- data.frame(
    metagenre = sample(
      c("rock", "pop", "jazz", "metal"),
      100,
      replace = TRUE
    )
  )
  test <- data.frame(
    metagenre = sample(
      c("rock", "pop", "jazz", "metal"),
      50,
      replace = TRUE
    )
  )
  cv_splits <- rsample::vfold_cv(
    data.frame(metagenre = train$metagenre),
    v = 3
  )

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)

  expect_type(result, "list")
  expect_s3_class(result$plot, "ggplot")

  split_types <- unique(result$proportions$split_type)
  expect_true("Train" %in% split_types)
  expect_true("Test" %in% split_types)
  expect_true("CV_Analysis" %in% split_types)
  expect_true("CV_Assessment" %in% split_types)
})

test_that("Integration: proportions sum to 1 per split", {
  train <- data.frame(
    metagenre = c("rock", "rock", "rock", "pop", "pop", "jazz")
  )
  test <- data.frame(
    metagenre = c("rock", "rock", "pop", "jazz", "jazz")
  )
  cv_splits <- rsample::vfold_cv(
    data.frame(metagenre = c("rock", "pop", "jazz")),
    v = 2
  )

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)
  proportions <- result$proportions

  for (split_type in unique(proportions$split_type)) {
    for (split_id in unique(
      proportions[proportions$split_type == split_type, ]$split_id
    )) {
      subset_sum <- sum(
        proportions[
          proportions$split_type == split_type &
            proportions$split_id == split_id,
        ]$proportion
      )
      expect_equal(subset_sum, 1, tolerance = 1e-10)
    }
  }
})

test_that("Integration: all metagenres appear in output", {
  train <- data.frame(
    metagenre = c("rock", "rock", "pop", "pop", "jazz")
  )
  test <- data.frame(
    metagenre = c("rock", "metal", "metal")
  )
  cv_splits <- NULL

  result <- compare_metagenre_distributions_splits(train, test, cv_splits)
  metagenres_in_output <- unique(result$proportions$metagenre)

  expect_true("rock" %in% metagenres_in_output)
  expect_true("pop" %in% metagenres_in_output)
  expect_true("jazz" %in% metagenres_in_output)
  expect_true("metal" %in% metagenres_in_output)
})
