#' Impute simulations object
#'
#' This function applies a user-supplied imputation function to each simulated dataset.
#'
#' @param sims_obj An object of class "simulations" (from simulate_missing_data).
#' @param impute_fun A function that takes a data.frame and returns an imputed data.frame.
#' @param alg_name Character. Name of the imputation algorithm for metadata (default NULL).
#' @param ... Additional arguments passed to impute_fun.
#' @return An object of class "imputations" containing:
#'   \item{simulations}{Original named list of simulated data.frames.}
#'   \item{imputations}{Named list of imputed data.frames (imp1, imp2, ...).}
#'   \item{meta}{List recording alg_name and extra args.}
#' @examples
#' # simple mean imputation
#' imputations <- impute_simulations(
#'   sims,
#'   impute_fun = function(df) {
#'     df[] <- lapply(df, function(col) {
#'       if (is.numeric(col)) col[is.na(col)] <- mean(col, na.rm = TRUE)
#'       col
#'     })
#'     df
#'   },
#'   alg_name = "mean imputation"
#' )
#' names(imputations$imputations)
#'
#' @importFrom utils head
#' @export
impute_simulations <- function(sims_obj, impute_fun, alg_name = NULL, ...) {
  if (!inherits(sims_obj, "simulations")) stop("`sims_obj` must be of class 'simulations'.")
  if (!is.function(impute_fun)) stop("`impute_fun` must be a function.")
  
  sims <- sims_obj$simulations
  imps <- lapply(seq_along(sims), function(i) {
    df <- sims[[i]]
    imp_df <- impute_fun(df, ...)
    if (!is.data.frame(imp_df)) stop("`impute_fun` must return a data.frame.")
    imp_df
  })
  
  # Name each imputed dataset imp1, imp2, ...
  names(imps) <- paste0("imp", seq_along(imps))
  
  structure(
    list(
      simulations = sims,
      imputations = imps,
      meta        = list(alg_name = alg_name, extra = list(...))
    ),
    class = "imputations"
  )
}

#' Print method for "imputations" objects
#'
#' @param x An object of class "imputations".
#' @param ... Additional arguments (ignored).
#' @export
print.imputations <- function(x, ...) {
  cat("imputations object:\n")
  cat("Number of imputations:", length(x$imputations), "
")
  if (!is.null(x$meta$alg_name)) cat("Algorithm:", x$meta$alg_name, "
")
  cat("Imputation names:", paste(names(x$imputations), collapse = ", "), "
")
  cat("First imputed dataset preview (5 rows):\n")
  print(utils::head(x$imputations[[1]], 5))
}