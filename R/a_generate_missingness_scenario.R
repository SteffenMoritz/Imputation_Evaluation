#' @title Generate Multiple Missing Data Scenarios Using a Custom Function
#'
#' @description This function creates multiple datasets with missing values using a user-defined missingness function (e.g., `missMethods::delete_MCAR`), and tracks the pattern of missing values across iterations.
#'
#' @param x A `data.frame`. The complete dataset to simulate missingness on.
#' @param unit_id A string or numeric column name/index used to uniquely identify rows (e.g., subject ID or timestamp).
#' @param n Integer. Number of missing data sets (iterations) to generate (default is 1).
#' @param miss_func A function to generate missing values (e.g., `missMethods::delete_MCAR`). This function should accept arguments like `data` and `prop`.
#' @param cols_mis A vector of column names or indices of columns in which missing values will be created.
#' @param prop Numeric. Proportion of values to make missing in each dataset (passed as `prop` to `miss_func`; default is 0.1).
#' @param ... Additional arguments passed to `miss_func` (e.g., `cols_mis`, `seed`, etc.).
#'
#' @return An object of class `"missingness_scenario"` containing:
#' \describe{
#'   \item{original_data}{The original, complete dataset.}
#'   \item{miss_iterations}{A `data.frame` tracking the position and variable of all simulated missing values across iterations.}
#'   \item{meta}{A list recording `unit_id`, `n`, `prop`, and any additional arguments passed through `...`.}
#' }
#'
#' @details
#' Each iteration of missingness is generated independently. The function preserves row order using `unit_id` to ensure consistent tracking. Internally, it copies the dataset to avoid side-effects. It also reshapes the pattern of missingness into a long format to enable easy analysis or visualization.
#'
#' @importFrom utils stack
#' @importFrom stats setNames
#' @export


a_generate_missingness_scenario <- function(x, unit_id, n = 1, miss_func, prop = 0.1, ...) {

  if (!is.data.frame(x)) stop("`x` must be a data.frame.")

  # Copy input to avoid side-effects
  x_copy <- x[order(x[[unit_id]]),]

  one_iteration <- function(i){
    gen_miss_i <- miss_func(
      data=x_copy,
      prop=prop,
      ...)

    gen_miss_i <- gen_miss_i[order(gen_miss_i[[unit_id]]),]

    gen_miss_i = data.frame(stats::setNames(list(gen_miss_i[[unit_id]]), unit_id),
                            is.na(gen_miss_i[,-1]) & !is.na(x_copy[,-1]))

    gen_miss_i <- gen_miss_i[rowSums(gen_miss_i[setdiff(names(gen_miss_i), unit_id)]) > 0, ]

    gen_miss_i$iter <- i

    # Ensure row names are removed before stacking and binding
    gen_miss_i <- data.frame(gen_miss_i, row.names = NULL)  # Reset row names

    id_and_iter <- c(unit_id, "iter")
    out_miss_i <- cbind(gen_miss_i[id_and_iter],
                        utils::stack(gen_miss_i[setdiff(names(gen_miss_i),
                                                 id_and_iter)]))[cbind(gen_miss_i[id_and_iter],
                                                                       utils::stack(gen_miss_i[setdiff(names(gen_miss_i),
                                                                                                id_and_iter)]))$values, ]

    out_miss_i <- out_miss_i[, setdiff(names(out_miss_i), "values")]
    names(out_miss_i)[names(out_miss_i) == "ind"] <- "variable"

    return(out_miss_i)
  }

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
