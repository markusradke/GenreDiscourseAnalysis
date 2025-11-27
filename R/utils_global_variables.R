utils::globalVariables(c(".data", ".from", ".to"))

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (!is.null(x)) x else y
  }
}
