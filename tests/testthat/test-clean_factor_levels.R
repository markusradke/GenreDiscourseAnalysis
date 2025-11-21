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


test_that("clean_factor_levels_in_folds remaps rare levels and
          returns cleaned objects", {
  train <- data.frame(
    id = 1:8,
    genre = factor(c("a", "a", "b", "b", "c", "c", "d", "d")),
    artist = factor(c("x", "x", "y", "y", "z", "z", "w", "w")),
    stringsAsFactors = TRUE
  )

  testd <- data.frame(
    id = 9:12,
    genre = factor(c("a", "c", "d", "e")),
    artist = factor(c("x", "z", "w", "q")),
    stringsAsFactors = TRUE
  )

  cv <- rsample::vfold_cv(train, v = 2)

  out <- suppressWarnings(clean_factor_levels_in_folds(
    cv,
    train,
    testd,
    min_n = 3
  ))

  expect_type(out, "list")
  expect_true(is.data.frame(out$train_data))
  expect_true(is.data.frame(out$test_data))

  facs <- names(out$train_data)[vapply(out$train_data, is.factor, logical(1))]

  for (f in facs) {
    lv <- levels(out$train_data[[f]])
    expect_false(length(lv) == 1 && lv[1] == "other")
  }
})

test_that("clean_factor_levels_in_folds is no-op when no factors", {
  train <- data.frame(a = 1:4, b = c(TRUE, FALSE, TRUE, FALSE))
  testd <- data.frame(a = 5:6, b = c(TRUE, TRUE))

  cv <- rsample::vfold_cv(train, v = 2)

  out <- clean_factor_levels_in_folds(cv, train, testd, min_n = 1)

  expect_equal(out$train_data, train)
  expect_equal(out$test_data, testd)
})
