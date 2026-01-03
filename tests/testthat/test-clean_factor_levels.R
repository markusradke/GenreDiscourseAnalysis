test_that("returns inputs unchanged when there are no factor columns", {
  train <- tibble::tibble(x = 1:5, y = rnorm(5))
  test <- tibble::tibble(x = 6:10, y = rnorm(5))
  cv <- tibble::tibble()

  out <- clean_factor_levels(cv, train, test, min_n = 2)

  expect_equal(out$train_data, train)
  expect_equal(out$test_data, test)
})

test_that("rare factor levels are remapped to 'other' across train, test and CV", {
  train <- tibble::tibble(
    id = 1:901,
    grp = factor(c(rep("g1", 600), rep("g2", 1), rep("g3", 300)))
  )
  test <- tibble::tibble(
    id = 1001:1100,
    grp = factor(c(rep("g2", 50), rep("g1", 50)))
  )
  cv_splits <- rsample::vfold_cv(train, v = 3)

  out <- clean_factor_levels(cv_splits, train, test, min_n = 2)

  # 'g2' should be removed and replaced by 'other'
  expect_false("g2" %in% levels(out$train_data$grp))
  expect_true("other" %in% levels(out$train_data$grp))
  expect_true("other" %in% levels(out$test_data$grp))

  # Counts of the remapped level equal the original rare counts
  expect_equal(301, sum(out$train_data$grp == "other"))
})

test_that("'other' below threshold is merged with smallest non-other level", {
  train <- tibble::tibble(
    id = 1:100,
    grp = factor(c(rep("a", 49), rep("b", 2), rep("c", 49)))
  )
  test <- tibble::tibble(
    id = 101:110,
    grp = factor(c(rep("b", 5), rep("a", 5)))
  )
  cv_splits <- rsample::vfold_cv(train, v = 3)

  # Use a higher threshold so 'b' becomes 'other' and then is below threshold
  out <- clean_factor_levels(cv_splits, train, test, min_n = 5)
  # 'b' should be gone and merged into the smallest non-other (here 'a')
  expect_false("b" %in% levels(out$train_data$grp))
  expect_true("other" %in% levels(out$train_data$grp))
  expect_equal(sum(out$train_data$grp == "other"), 51)
})

test_that("columns that end up with only 'other' are removed", {
  # Make two factor columns where all levels are below threshold
  train <- tibble::tibble(
    id = 1:6,
    f1 = factor(c("r1", "r2", "r3", "r4", "r5", "r6")),
    f2 = factor(rep("x", 6))
  )
  test <- tibble::tibble(
    id = 7:8,
    f1 = factor(c("r1", "r2")),
    f2 = factor(c("x", "x"))
  )
  cv_splits <- rsample::vfold_cv(train, v = 2)

  # Set threshold above any per-partition counts so all factor levels map to 'other'
  out <- suppressWarnings(clean_factor_levels(
    cv_splits,
    train,
    test,
    min_n = 2
  ))

  # f1 should be removed (only 'other' would remain), f2 kept
  expect_false("f1" %in% names(out$train_data))
  expect_true("f2" %in% names(out$train_data))
})

test_that("remap_rare_levels_in_splits rebuilds CV rset and preserves metadata", {
  train <- tibble::tibble(
    id = 1:9,
    grp = factor(c(rep("a", 4), rep("b", 3), rep("c", 2)))
  )
  cv_splits <- rsample::vfold_cv(train, v = 3)

  # Simulate remapping 'c' -> 'other' inside splits
  remapped <- remap_rare_levels_in_splits(cv_splits, list(grp = "c"))

  # Structure preserved: same number of resamples and same rset class
  expect_equal(nrow(remapped), nrow(cv_splits))
  expect_equal(class(remapped), class(cv_splits))
})

test_that("remap_factor_column creates 'other' and drops specified levels", {
  df <- tibble::tibble(x = factor(c("A", "B", "C", "A")))
  out <- remap_factor_column(df, "x", c("B"))

  expect_true("other" %in% levels(out$x))
  expect_false("B" %in% levels(out$x))
  expect_equal(sum(out$x == "other"), 1)
})

test_that("get_factor_columns excludes metagenre and non-factors", {
  df <- data.frame(
    a = factor(c("x", "y")),
    b = factor(c("u", "v")),
    metagenre = factor(c("m", "n")),
    c = 1:2,
    stringsAsFactors = TRUE
  )

  res <- get_factor_columns(df)

  expect_true(all(c("a", "b") %in% res))
  expect_false("metagenre" %in% res)
})

test_that("remap_factor_column maps listed levels to other and keeps factors", {
  df <- data.frame(
    g = factor(c("rock", "pop", "jazz", "rock")),
    stringsAsFactors = TRUE
  )

  out <- remap_factor_column(df, "g", c("jazz"))

  expect_true(is.factor(out$g))
  expect_true(any(as.character(out$g) == "other"))
  expect_false(any(as.character(out$g) == "jazz"))
  expect_equal(
    out$g,
    factor(
      c("rock", "pop", "other", "rock"),
      levels = c("pop", "rock", "other")
    )
  )
})

test_that("remap_rare_levels_in_data applies remapping for multiple cols", {
  df <- data.frame(
    a = factor(c("x", "y", "z")),
    b = factor(c("u", "v", "u")),
    stringsAsFactors = TRUE
  )

  rare <- list(a = c("z"), b = c("v"))

  out <- remap_rare_levels_in_data(df, rare)

  expect_true("other" %in% levels(out$a))
  expect_true("other" %in% levels(out$b))
  expect_false(any(as.character(out$a) == "z"))
  expect_false(any(as.character(out$b) == "v"))
})

test_that("identify_rare_levels_in_partitions finds levels below threshold", {
  p1 <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  p2 <- factor(c("a", "c", "a"), levels = c("a", "b", "c"))
  p3 <- factor(c("b", "b", "b"), levels = c("a", "b", "c"))

  rare <- identify_rare_levels_in_partitions(
    list(p1, p2, p3),
    min_n = 2
  )
  expect_true(all(c("b", "c") %in% rare))
})

test_that("collect_all_partitions returns analysis/assessment and datasets", {
  df <- data.frame(
    id = 1:6,
    f = factor(c("a", "a", "b", "b", "c", "c")),
    stringsAsFactors = TRUE
  )

  cv <- rsample::vfold_cv(df, v = 2)

  parts <- collect_partitions_for_column(cv, df, df, "f")

  expect_true(length(parts) >= 3)
  expect_true(is.factor(parts[[1]]))
})

test_that("remap_rare_levels_in_splits rebuilds splits and keeps ids", {
  df <- data.frame(
    id = 1:6,
    g = factor(c("x", "x", "y", "y", "z", "z")),
    stringsAsFactors = TRUE
  )

  cv <- rsample::vfold_cv(df, v = 2)

  rare <- list(g = c("z"))

  new_cv <- remap_rare_levels_in_splits(cv, rare)

  expect_s3_class(new_cv, class(cv))
  expect_equal(nrow(new_cv), nrow(cv))
  first_split <- rsample::analysis(new_cv$splits[[1]])
  expect_true(is.factor(first_split$g))
})

test_that("find_single_level_factors detects only-other factors", {
  df <- data.frame(
    a = factor(c("other", "other")),
    b = factor(c("x", "y")),
    stringsAsFactors = TRUE
  )

  res <- find_single_level_factors(df, c("a", "b"))

  expect_equal(res, "a")
})

test_that("remove_columns drops selected columns", {
  df <- data.frame(x = 1:3, y = 4:6)

  out <- remove_columns(df, "y")

  expect_false("y" %in% names(out))
  expect_true("x" %in% names(out))
})


test_that("clean_factor_levels_in_folds is no-op when no factors", {
  train <- data.frame(a = 1:4, b = c(TRUE, FALSE, TRUE, FALSE))
  testd <- data.frame(a = 5:6, b = c(TRUE, TRUE))

  cv <- rsample::vfold_cv(train, v = 2)

  out <- clean_factor_levels(cv, train, testd, min_n = 1)

  expect_equal(out$train_data, train)
  expect_equal(out$test_data, testd)
})
