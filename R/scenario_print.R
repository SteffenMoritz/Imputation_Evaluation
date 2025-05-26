
#' Print method for "missingness_scenario" objects
#'
#' @param x An object of class "missingness_scenario".
#' @param ... Additional arguments (ignored).
#' @export
print.missingness_scenario <- function(x, ...) {
  cat("miss_iterations object: ", length(x$miss_iterations), " datasets\n")
  cat("prop =", x$meta$prop, "\n")
  cat("Datasets names: ", paste(names(x), collapse = ", "), "\n")
  cat("First dataset preview (5 rows):\n")
  print(utils::head(x$miss_iterations[[1]], 5))
}



scenario_print <- function(x, ...) {
  cat("miss_iterations object: ", length(x$miss_iterations), " datasets\n")
  cat("prop =", x$meta$prop, "\n")
  cat("Datasets names: ", paste(names(x), collapse = ", "), "\n")
  cat("First dataset preview (5 rows):\n")
  print(utils::head(x$miss_iterations[[1]], 5))
}