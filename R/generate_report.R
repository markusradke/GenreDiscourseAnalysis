generate_report <- function(qmd_in_inst) {
  qmd_file <- system.file(
    sprintf("%s.qmd", qmd_in_inst),
    package = "GenreDiscourseAnalysis"
  )
  if (!file.exists(qmd_file)) {
    stop(
      sprintf(
        "Cannot find the .qmd file for report generation: '%s'",
        qmd_in_inst
      )
    )
  }
  quarto_render_move(
    input = qmd_file,
    output_dir = "reports"
  )
  message(sprintf(
    "Report '%s' generated in 'reports/' directory.",
    qmd_file
  ))
}

#' `quarto::quarto_render()`, but output file is moved to `output_dir`
#'
#' The default `quarto::quarto_render()` function can only render outputs
#' to the current working directory. This is a wrapper that moves the rendered
#' output to `output_dir`.
#' @param input Path to the input qmd file.
#' @param output_dir Path to the output directory.
#' @param ... Other args passed to `quarto::quarto_render()`
quarto_render_move <- function(
  input,
  output_dir = NULL,
  ...
) {
  paths <- get_paths_from_qmd(input, output_dir)
  quarto::quarto_render(input = input, ... = ...)
  move_rendered_report(paths)
}

get_paths_from_qmd <- function(input, output_dir = NULL) {
  input_qmd <- quarto::quarto_inspect(input)
  output_format <- names(input_qmd$formats)
  output <- input_qmd$formats[[output_format]]$pandoc$`output-file`
  output_file <- output
  input_dir <- dirname(input)
  if (is.null(output_dir)) {
    output_dir <- input_dir
  }
  output_path_from <- file.path(input_dir, output)
  output_path_to <- file.path(output_dir, output_file)
  return(list(
    output_format = output_format,
    output_file = output_file,
    input_dir = input_dir,
    output_dir = output_dir,
    output_path_from = output_path_from,
    output_path_to = output_path_to
  ))
}

move_rendered_report <- function(paths) {
  if (paths$input_dir != paths$output_dir) {
    move_output_file(paths)
    if (paths$output_format == "html") {}
    move_supporting_files(paths)
  }
}

move_output_file <- function(paths) {
  if (!dir.exists(paths$output_dir)) {
    dir.create(paths$output_dir)
  }
  file.copy(
    from = paths$output_path_from,
    to = paths$output_path_to,
    overwrite = TRUE
  )
  file.remove(paths$output_path_from)
}

move_supporting_files <- function(paths) {
  supporting_files_from <- file.path(
    paths$input_dir,
    paste0(tools::file_path_sans_ext(paths$output_file), "_files")
  )
  supporting_files_to <- file.path(
    paths$output_dir,
    paste0(tools::file_path_sans_ext(paths$output_file), "_files")
  )
  if (dir.exists(supporting_files_from)) {
    if (dir.exists(supporting_files_to)) {
      unlink(supporting_files_to, recursive = TRUE)
    }
    file.rename(
      from = supporting_files_from,
      to = supporting_files_to
    )
  }
}
