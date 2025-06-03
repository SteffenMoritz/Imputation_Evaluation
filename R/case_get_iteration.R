#' Geta particular iteration of an imputation case
#'
#' This function imputes missing values in a dataset based on imputation values from another dataset.
#' It allows specifying the iteration (`iter`) and a dynamic identifier column (`business_col`).
#'
#' @param x A data frame containing the original dataset.
#' @param imputation_case A data frame containing the imputed values, including iteration and variable names.
#' @param unit_id A string specifying the column name used as an identifier.
#' @param iter An integer specifying which iteration to apply.
#'
#' @return A data frame with imputed values for the specified iteration.
#'
#' @export
case_get_iteration <- function(x, imputation_case, unit_id, iter) {
  # Validate that the iteration exists
  if (!(iter %in% imputation_case$iter)) {
    stop(paste("Error: Iteration", iter, "does not exist in imputation_case."))
  }

  # Filter imputations for the given iteration
  imp_sub <- imputation_case[imputation_case$iter == iter, ]

  # Create a copy of the original data to modify
  imputed <- x

  # Add iteration column
  imputed$iter <- iter

  # Apply imputations
  for (i in seq_len(nrow(imp_sub))) {
    row_idx <- imputed[[unit_id]] == imp_sub[[unit_id]][i]
    var_name <- imp_sub$variable[i]
    imputed[row_idx, var_name] <- imp_sub$value[i]
  }

  return(imputed)
}
