write_test_qmd <- function() {
  temp_dir <- tempdir()
  temp_qmd <- file.path(temp_dir, "test_report.qmd")
  qmd_content <- '---
title: "Test Report"
format: 
  html:
    output-file: "test_output.html"
---

# Test Content
This is a test report.
'
  writeLines(qmd_content, temp_qmd)
}

test_that("get_paths_from_qmd extracts correct paths", {
  skip_if_not_installed("quarto")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not available")

  write_test_qmd()
  temp_dir <- tempdir()
  temp_qmd <- file.path(temp_dir, "test_report.qmd")
  paths <- get_paths_from_qmd(temp_qmd, output_dir = "reports")
  expect_type(paths, "list")
  expect_true(all(
    c(
      "output_format",
      "output_file",
      "input_dir",
      "output_dir",
      "output_path_from",
      "output_path_to"
    ) %in%
      names(paths)
  ))
  # TODO clean file pahths for cross-platform compatibility
  expect_equal(paths$output_format, "html")
  expect_equal(paths$output_file, "test_output.html")
  expect_equal(gsub("\\\\", "/", paths$input_dir), gsub("\\\\", "/", temp_dir))
  expect_equal(paths$output_dir, "reports")
  expect_equal(
    gsub("\\\\", "/", paths$output_path_from),
    gsub("\\\\", "/", file.path(temp_dir, "test_output.html"))
  )
  expect_equal(
    gsub("\\\\", "/", paths$output_path_to),
    gsub("\\\\", "/", file.path("reports", "test_output.html"))
  )

  unlink(temp_qmd)
})

test_that("get_paths_from_qmd uses input_dir as output_dir if NULL", {
  skip_if_not_installed("quarto")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not available")

  temp_dir <- tempdir()
  write_test_qmd()
  temp_qmd <- file.path(temp_dir, "test_report.qmd")
  paths <- get_paths_from_qmd(temp_qmd, output_dir = NULL)

  expect_equal(paths$output_dir, paths$input_dir)
  expect_equal(
    gsub("\\\\", "/", paths$output_path_to),
    gsub("\\\\", "/", file.path(temp_dir, "test_output.html"))
  )

  unlink(temp_qmd)
})

test_that("move_output_file creates output directory and moves file", {
  temp_input_dir <- file.path(tempdir(), "input")
  temp_output_dir <- file.path(tempdir(), "output")

  dir.create(temp_input_dir, showWarnings = FALSE)

  test_file <- file.path(temp_input_dir, "test.html")
  writeLines("test content", test_file)

  paths <- list(
    output_dir = temp_output_dir,
    output_path_from = test_file,
    output_path_to = file.path(temp_output_dir, "test.html")
  )

  expect_false(dir.exists(temp_output_dir))
  move_output_file(paths)
  expect_true(dir.exists(temp_output_dir))
  expect_true(file.exists(file.path(temp_output_dir, "test.html")))
  expect_false(file.exists(test_file))
  content <- readLines(file.path(temp_output_dir, "test.html"))
  expect_equal(content, "test content")

  unlink(temp_input_dir, recursive = TRUE)
  unlink(temp_output_dir, recursive = TRUE)
})

test_that("move_output_file overwrites existing files", {
  temp_input_dir <- file.path(tempdir(), "input2")
  temp_output_dir <- file.path(tempdir(), "output2")

  dir.create(temp_input_dir, showWarnings = FALSE)
  dir.create(temp_output_dir, showWarnings = FALSE)

  source_file <- file.path(temp_input_dir, "test.html")
  writeLines("new content", source_file)
  target_file <- file.path(temp_output_dir, "test.html")
  writeLines("old content", target_file)

  paths <- list(
    output_dir = temp_output_dir,
    output_path_from = source_file,
    output_path_to = target_file
  )

  move_output_file(paths)
  content <- readLines(target_file)

  expect_equal(content, "new content")
  expect_false(file.exists(source_file))

  unlink(temp_input_dir, recursive = TRUE)
  unlink(temp_output_dir, recursive = TRUE)
})

test_that("move_supporting_files moves _files directory", {
  temp_input_dir <- file.path(tempdir(), "input3")
  temp_output_dir <- file.path(tempdir(), "output3")

  dir.create(temp_input_dir, showWarnings = FALSE)
  dir.create(temp_output_dir, showWarnings = FALSE)

  supporting_dir <- file.path(temp_input_dir, "test_files")
  dir.create(supporting_dir)

  writeLines("css content", file.path(supporting_dir, "style.css"))
  writeLines("js content", file.path(supporting_dir, "script.js"))

  paths <- list(
    input_dir = temp_input_dir,
    output_dir = temp_output_dir,
    output_file = "test.html"
  )

  move_supporting_files(paths)

  target_supporting_dir <- file.path(temp_output_dir, "test_files")
  expect_true(dir.exists(target_supporting_dir))
  expect_false(dir.exists(supporting_dir))
  expect_true(file.exists(file.path(target_supporting_dir, "style.css")))
  expect_true(file.exists(file.path(target_supporting_dir, "script.js")))

  css_content <- readLines(file.path(target_supporting_dir, "style.css"))
  expect_equal(css_content, "css content")

  unlink(temp_input_dir, recursive = TRUE)
  unlink(temp_output_dir, recursive = TRUE)
})

test_that("move_supporting_files removes existing target directory", {
  temp_input_dir <- file.path(tempdir(), "input4")
  temp_output_dir <- file.path(tempdir(), "output4")

  dir.create(temp_input_dir, showWarnings = FALSE)
  dir.create(temp_output_dir, showWarnings = FALSE)

  source_supporting_dir <- file.path(temp_input_dir, "test_files")
  dir.create(source_supporting_dir)
  writeLines("new content", file.path(source_supporting_dir, "file.txt"))

  target_supporting_dir <- file.path(temp_output_dir, "test_files")
  dir.create(target_supporting_dir)
  writeLines("old content", file.path(target_supporting_dir, "old_file.txt"))

  paths <- list(
    input_dir = temp_input_dir,
    output_dir = temp_output_dir,
    output_file = "test.html"
  )

  move_supporting_files(paths)

  expect_true(dir.exists(target_supporting_dir))
  expect_false(dir.exists(source_supporting_dir))
  expect_true(file.exists(file.path(target_supporting_dir, "file.txt")))
  expect_false(file.exists(file.path(target_supporting_dir, "old_file.txt")))

  unlink(temp_input_dir, recursive = TRUE)
  unlink(temp_output_dir, recursive = TRUE)
})

test_that("move_supporting_files does nothing when no supporting files exist", {
  temp_input_dir <- file.path(tempdir(), "input5")
  temp_output_dir <- file.path(tempdir(), "output5")

  dir.create(temp_input_dir, showWarnings = FALSE)
  dir.create(temp_output_dir, showWarnings = FALSE)

  paths <- list(
    input_dir = temp_input_dir,
    output_dir = temp_output_dir,
    output_file = "test.html"
  )
  expect_silent(move_supporting_files(paths))

  target_supporting_dir <- file.path(temp_output_dir, "test_files")
  expect_false(dir.exists(target_supporting_dir))

  unlink(temp_input_dir, recursive = TRUE)
  unlink(temp_output_dir, recursive = TRUE)
})

test_that("quarto_render_move integrates all components", {
  skip_if_not_installed("quarto")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not available")

  # Create a minimal qmd file for rendering
  temp_dir <- tempdir()
  temp_qmd <- file.path(temp_dir, "test_report.qmd")
  temp_output_dir <- file.path(temp_dir, "reports")
  write_test_qmd()
  quarto_render_move(
    input = temp_qmd,
    output_dir = temp_output_dir
  )
  expect_true(file.exists(file.path(temp_output_dir, "test_output.html")))
  expect_false(file.exists(file.path(temp_dir, "test_output.html")))

  html_content <- suppressWarnings(readLines(file.path(
    temp_output_dir,
    "test_output.html"
  )))
  html_text <- paste(html_content, collapse = "\n")
  expect_true(grepl("This is a test report.", html_text))

  unlink(temp_qmd)
  unlink(temp_output_dir, recursive = TRUE)
})

test_that("generate_report throws error when qmd file not found", {
  expect_error(
    generate_report("non_existent_report"),
    "Cannot find the .qmd file for report generation"
  )
})

test_that("paths construction handles different file extensions", {
  skip_if_not_installed("quarto")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not available")

  temp_dir <- tempdir()
  pdf_qmd <- file.path(temp_dir, "test_pdf.qmd")
  pdf_content <- '---
title: "PDF Test"
format: 
  pdf:
    output-file: "test_output.pdf"
---

# PDF Content
'

  writeLines(pdf_content, pdf_qmd)
  paths <- get_paths_from_qmd(pdf_qmd, "output")

  expect_equal(paths$output_format, "pdf")
  expect_equal(paths$output_file, "test_output.pdf")
  expect_true(grepl("\\.pdf$", paths$output_path_to))

  unlink(pdf_qmd)
})
