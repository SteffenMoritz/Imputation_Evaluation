#' Simulate missing data via missMethods::delete_MCAR
#'
#' This function creates multiple datasets with MCAR missing data using the missMethods package.
#'
#' @param x A data.frame.
#' @param n Number of datasets to generate (default 1).
#' @param prop Proportion of values to delete (default 0.1).
#' @param ... Additional arguments passed to delete_MCAR (e.g., seed).
#' @return An object of class "simulations" containing:
#'   \item{simulations}{Named list of data.frames (sim1, sim2, ...) with introduced missingness.}
#'   \item{meta}{List recording n, prop, and extra args.}
#' @details
#' The original data.frame is copied internally to avoid side-effects on the input.
#' @examples
#' library(missMethods)
#' data(airquality)
#' sims <- simulate_missing_data(airquality, n = 3, prop = 0.2, seed = 123)
#' names(sims$simulations)
#'
#' @import missMethods
#' @importFrom utils head
#' @export
simulate_missing_data <- function(x, n = 1, prop = 0.1, ...) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame.")
  if (!requireNamespace("missMethods", quietly = TRUE)) stop("Please install the 'missMethods' package.")
  
  # Copy input to avoid side-effects
  x_copy <- x
  
  # Generate n datasets with MCAR missingness
  sims <- lapply(seq_len(n), function(i) {
    missMethods::delete_MCAR(
      data = x_copy,
      propMissing = prop,
      ...
    )
  })
  
  # Name each dataset sim1, sim2, ...
  names(sims) <- paste0("sim", seq_len(n))
  
  structure(
    list(
      simulations = sims,
      meta = list(n = n, prop = prop, extra = list(...))
    ),
    class = "simulations"
  )
}

#' Print method for "simulations" objects
#'
#' @param x An object of class "simulations".
#' @param ... Additional arguments (ignored).
#' @export
print.simulations <- function(x, ...) {
  cat("simulations object: ", length(x$simulations), " datasets\n")
  cat("prop =", x$meta$prop, "\n")
  cat("Datasets names: ", paste(names(x$simulations), collapse = ", "), "\n")
  cat("First dataset preview (5 rows):\n")
  print(utils::head(x$simulations[[1]], 5))
}
