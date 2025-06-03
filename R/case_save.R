#' Save imputation case as Parquet file
#'
#' This function extracts the `imputations` element from a `imputation_case` object
#' then saves it as a Parquet file at the specified location.
#'
#' @param x A `data.frame`. The complete dataset on which missigness and imputation are going to be generated.
#' @param imputation_case A list containing two elements: `imputations` and `meta`.
#' @param save_path A string specifying the file path to save the Parquet file.
#' @param unit_id A string or numeric column name/index used to uniquely identify rows (e.g., subject ID or timestamp).
#' @param optimized A logical indicating if optimized version of specifying of `imputations` is desired.
#'
#' @return None. Saves the `imputations` data frame as a Parquet file.
#'
#' @importFrom arrow write_parquet
#' @examples
#' library(arrow)
#' imputation_case <- list(imputations = data.frame(ID = c(1, 2, 3), value = c(NA, 5, 8)),
#'                         meta = NULL)
#' case_save(imputation_case$imputations, "imp_case.parq")
#'
#' @export
#'
case_save <- function(x, imputation_case, save_path, unit_id, optimized=FALSE) {
  # Load the required package
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Please install it using install.packages('arrow').")
  }

  # Extract the miss_iterations element
  imp_iterations <- imputation_case$imputations

  # Validate the extracted data
  if (!is.data.frame(imp_iterations)) {
    stop("The 'imp_iterations' element should be a data frame.")
  }

  if (optimized){
    # Save the data frame as a Parquet file
    arrow::write_parquet(imp_iterations, save_path)

    message("File saved successfully at: ", save_path)

  } else {

    n <- max(imp_iterations$iter)

    iterations <- do.call(rbind,
                          lapply(seq_len(n),
                                 function(l) case_get_iteration(x, imp_iterations, unit_id, l)))

    arrow::write_parquet(iterations, save_path)

    message("One iteration file saved successfully at: ", save_path)
  }

}
