#' Simulate missing data via missMethods::delete_MCAR
#'
#' This function creates multiple datasets with MCAR missing data using the missMethods package.
#'
#' @param x A data.frame.
#' @param n Number of datasets to generate (default 1).
#' @param prop Proportion of values to delete (passed to `p` in delete_MCAR; default 0.1).
#' @param cols_mis A vector of column names or indices of columns in which missing values will be created.
#' @param ... Additional arguments passed to delete_MCAR (e.g., seed).
#' @return An object of class "simulations" containing:
#'   \item{miss_trials}{Dataframe of missing trials.}
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
#' @import mice
#' @importFrom utils head
#' @export
simulate_missing_data <- function(x, unit_id, n = 1, miss_func, prop = 0.1, ...) {

  if (!is.data.frame(x)) stop("`x` must be a data.frame.")
  if (!requireNamespace("missMethods", quietly = TRUE)) stop("Please install the 'missMethods' package.")
  
  # Copy input to avoid side-effects
  x_copy <- x[order(x[[unit_id]]),]

  one_trial <- function(i){
    gen_miss_i <- miss_func(
      data=x_copy, 
      prop=prop,
      ...)
    
    gen_miss_i <- gen_miss_i[order(gen_miss_i[[unit_id]]),]
    
    gen_miss_i = data.frame(setNames(list(gen_miss_i[[unit_id]]), unit_id), 
                            is.na(gen_miss_i[,-1]) & !is.na(data[,-1]))
    gen_miss_i$trial <- i
    
    return(gen_miss_i)
  }
  gen_miss <- map_dfr(seq_len(n), one_trial)

  
  # Rearrange dataframe column order to have trial in second position after unique identifier
  gen_miss <- gen_miss[,c(1, ncol(gen_miss), 2:(ncol(gen_miss)-1) )]
  # Reset the row index
  rownames(gen_miss) <- NULL
  
  structure(
    list(
      miss_trials = gen_miss,
      meta = list(n = n, prop = prop, extra = list(...))

    ),
    class = "miss_trials"
  )
}

#' Print method for "simulations" objects
#'
#' @param x An object of class "simulations".
#' @param ... Additional arguments (ignored).
#' @export
print.miss_trials <- function(x, ...) {
  cat("miss_trials object: ", length(x$miss_trials), " datasets\n")
  cat("prop =", x$meta$prop, "\n")
  cat("Datasets names: ", paste(names(x$miss_trials), collapse = ", "), "\n")
  cat("First dataset preview (5 rows):\n")
  print(utils::head(x$miss_trials[[1]], 5))
}
