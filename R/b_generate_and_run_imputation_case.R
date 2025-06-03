#' Impute a Missingness Scenario with a Custom Function
#'
#' This function applies a user-supplied imputation function to each simulated dataset stored in a `missingness_scenario` object.
#'
#' @param missingness_scenario An object of class `"missingness_scenario"` as returned by `a_generate_missingness_scenario()`.
#' @param out_optimized A logical indicating if optimized version is desired or not.
#' @param impute_fun A function that takes a `data.frame` and returns an imputed `data.frame`. This function will be applied to each simulated dataset.
#' @param alg_name Character. Optional name of the imputation algorithm, stored in the output metadata (default is `NULL`).
#' @param ... Additional arguments passed to `impute_fun`.
#'
#' @return An object of class `"imputation_case"` containing:
#' \describe{
#'   \item{imputations}{A named list of imputed data.frames (`imp1`, `imp2`, ...).}
#'   \item{meta}{A list storing `alg_name` and any additional arguments passed through `...`.}
#' }
#'
#' @details
#' By default, imputation is run in single-threaded mode. Parallel execution can be added by modifying the internal flag or extending the function.
#'
#' @examples
#' # Example: mean imputation
#' imputations <- b_generate_and_run_imputation_case(
#'   missingness_scenario = sims,
#'   impute_fun = function(df) {
#'     df[] <- lapply(df, function(col) {
#'       if (is.numeric(col)) col[is.na(col)] <- mean(col, na.rm = TRUE)
#'       col
#'     })
#'     df
#'   },
#'   alg_name = "Mean Imputation"
#' )
#' names(imputations$imputations)
#'
#' @importFrom utils head
#' @import parallel
#' @export

b_generate_and_run_imputation_case <- function(missingness_scenario, out_optimized=TRUE, impute_fun, alg_name = NULL, ...) {
  if (!inherits(missingness_scenario, "missingness_scenario")) stop("`missingness_scenario` must be of class 'missingness_scenario'.")
  if (!is.function(impute_fun)) stop("`impute_fun` must be a function.")

  parallel <- F

  if (parallel == T ) {
    n_cores <- max(1, detectCores(logical = FALSE) - 1)

    cl <- makeCluster(n_cores)

    # Export needed objects/functions
    clusterExport(cl, varlist = c("sims", "impute_fun"), envir = environment())

    # Load packages inside workers if needed

    imps <- parLapply(cl, seq_len(n_iter), function(i) {
      df <- scenario_get_iteration(sims, i)
      imp_df <- impute_fun(df, ...)
      if (!is.data.frame(imp_df)) stop("`impute_fun` must return a data.frame.")
      imp_df
    })

    stopCluster(cl)

  } else {
    message("Only one core available — falling back to single-threaded mode.")
    imps <- do.call(rbind, lapply(seq_len(missingness_scenario$meta$n), function(i) {
      df <- scenario_get_iteration(missingness_scenario, i)$scenario_iteration
      imp_df <- impute_fun(df, ...)
      if (!is.data.frame(imp_df)) stop("`impute_fun` must return a data.frame.")
      if (out_optimized){
        out_create_from_case(missingness_scenario$original_data, imp_df, missingness_scenario$meta$unit_id)
      }else{
        imp_df
      }
    })
    )
  }

  structure(
    list(
      imputations = imps,
      meta        = list(missingness_scenario = missingness_scenario,
                         alg_name = alg_name,
                         extra = list(...))
    ),
    class = "imputation_case"
  )
}

#' Print method for "imputations" objects
#'
#' @param x An object of class "imputations".
#' @param ... Additional arguments (ignored).
#' @export
print.imputation_case <- function(x, ...) {
  cat("imputations object:\n")
  if (!is.null(x$meta$alg_name)) cat("Algorithm:", x$meta$alg_name, "
")
  cat("Imputation names:", paste(names(x$imputations), collapse = ", "), "
")
  cat("First imputed dataset preview (5 rows):\n")
  print(utils::head(x$imputations, 5))
}


