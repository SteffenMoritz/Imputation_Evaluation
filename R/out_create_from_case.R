#' Identify and Extract Changed Values Between Original and Imputed DataFrames
#'
#' This function compares two data frames (original and imputed) and extracts only records and values of what changed.
#'
#' @param x A data frame containing original values.
#' @param imputed A data frame containing imputed values with the same structure as original, plus an iteration column `iter`.
#' @param unit_id A character string specifying the column name representing unique identifiers.
#'
#' @return A data frame with columns "unit_id", "iter", "variable", and "value", containing only changed values, sorted by iter and unit_id.
#'
#' @examples
#' x <- data.frame(unit_id = c(1,2,3,4), variable_a = c(10,20,30,40), variable_b = c(5,15,25,35))
#' imputed <- data.frame(unit_id = c(1,2,3,4,5), iter = c(1,1,1,1,1), variable_a = c(10,25,30,45,50), variable_b = c(5,15,20,35,40))
#' detect_changes(x, imputed, "unit_id")
#'
#' @export


out_create_from_case <- function(x, imputed, unit_id) {

  # Ensure one iteration exists on imputed each run
  if (length(unique(imputed$iter)) > 1) {
    stop("imputed creation cannot be performed since Imputed data.frame contains more than one iteration.
  Please ensure a unique iteration exists before running 'out_create_from_case'.")
  }

  # Ensure original contains all (unit_id, iter) pairs present in imputed
  two_ids <- c(unit_id, "iter")
  unit_id_imp <- imputed[, unit_id, drop = FALSE]

  original <- merge(unit_id_imp, x, by = unit_id, all.x = TRUE)
  imputed <- imputed[order(imputed[[unit_id]]), ]

  # Identify changed values
  changes <- original[, setdiff(names(original), unit_id)] != imputed[, setdiff(names(imputed), two_ids)]

  # Get variable names for changed values
  changed_vars <- rep(setdiff(names(original), unit_id), each = nrow(original))[changes]

  # Create a DataFrame with changed values
  out_imp <- setNames(
    data.frame(
      rep(original[[unit_id]], times = ncol(original) - 1)[changes],
      iter = unique(imputed$iter),
      variable = changed_vars,
      value = unlist(imputed[, setdiff(names(imputed), two_ids)])[changes]
    ),
    c(unit_id, "iter", "variable", "value")
  )

  # Sort by iter and unit_id before returning
  out_imp <- out_imp[order(out_imp$iter, out_imp[[unit_id]]), ]

  rownames(out_imp) <- NULL # Remove row indices

  return(out_imp)
}

