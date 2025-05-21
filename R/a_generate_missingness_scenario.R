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
#'   \item{miss_iterations}{Dataframe of missing iterations.}
#'   \item{original_data}{Dataframe of original data.}
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
a_generate_missingness_scenario <- function(x, unit_id, n = 1, miss_func, prop = 0.1, ...) {
  
  if (!is.data.frame(x)) stop("`x` must be a data.frame.")
  if (!requireNamespace("missMethods", quietly = TRUE)) stop("Please install the 'missMethods' package.")
  
  # Copy input to avoid side-effects
  x_copy <- x[order(x[[unit_id]]),]
  
  one_iteration <- function(i){
    gen_miss_i <- miss_func(
      data=x_copy, 
      prop=prop,
      ...)
    
    gen_miss_i <- gen_miss_i[order(gen_miss_i[[unit_id]]),]
    
    gen_miss_i = data.frame(setNames(list(gen_miss_i[[unit_id]]), unit_id), 
                            is.na(gen_miss_i[,-1]) & !is.na(x_copy[,-1]))
    
    gen_miss_i <- gen_miss_i[rowSums(gen_miss_i[setdiff(names(gen_miss_i), unit_id)]) > 0, ]
    
    gen_miss_i$iter <- i
    
    # Ensure row names are removed before stacking and binding
    gen_miss_i <- data.frame(gen_miss_i, row.names = NULL)  # Reset row names
    
    id_and_iter <- c(unit_id, "iter")
    out_miss_i <- cbind(gen_miss_i[id_and_iter], 
                        stack(gen_miss_i[setdiff(names(gen_miss_i), 
                                                 id_and_iter)]))[cbind(gen_miss_i[id_and_iter], 
                                                                       stack(gen_miss_i[setdiff(names(gen_miss_i), 
                                                                                                id_and_iter)]))$values, ]
    
    out_miss_i <- out_miss_i[, setdiff(names(out_miss_i), "values")]
    names(out_miss_i)[names(out_miss_i) == "ind"] <- "variable"
    
    return(out_miss_i)
  }
  
  # out_miss <- map_dfr(seq_len(n), one_iteration)
  
  out_miss <- do.call(rbind, lapply(seq_len(n), one_iteration))
  
  # Reset the row index
  rownames(out_miss) <- NULL
  
  structure(
    list(
      original_data = x,
      miss_iterations = out_miss,
      meta = list(unit_id = unit_id, n = n, prop = prop, extra = list(...))
      
    ),
    class = "missingness_scenario"
  )
}

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
