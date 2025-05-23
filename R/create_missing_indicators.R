#' Add Missingness Indicators to Original Data
#'
#' This function creates a new dataframe that includes all columns from `original_data`
#' and additional columns indicating missingness for each variable found in `missing_iterations`.
#'
#' @param missingness_scenario A list containing three elements: `original_data`, `miss_iterations` and `meta`.
#'
#' @return A dataframe that includes `original_data`, `iter`, and missingness indicators for variables that 
#' have a non-zero number of missing generations.
#'
#' @examples
#' missingness_scenario <- list(
#'             original_data = data.frame(ID = c(1, 2, 3, 4), var1 = c(7, 5, 9, 11), var2 = c(13, 21, 7, 15)),
#'             miss_iterations = data.frame(ID = c(1, 1, 2, 1, 2), iter = c(1, 3, 1, 2, 2),
#'                           variable = c('var1', 'var2', 'var1', 'var1', 'var1')),
#'             meta = list(unit_id = "business_number"))
#' create_missingness_df(original_data)
#'
#' @export
create_missing_indicators <- function(missingness_scenario) {
  
  original_data <- missingness_scenario$original_data
  missing <- missingness_scenario$miss_iterations
  meta <- missingness_scenario$meta
  unit_id <- meta$unit_id
  
  
  # Identify unique iter values from missing_ind
  unique_iters <- unique(missing$iter)
  
  # Generate all possible (ID, iter) combinations
  missing_indicators <- expand.grid(unique(original_data[[unit_id]]), unique_iters)
  missing_indicators <- setNames(missing_indicators, c(unit_id, "iter"))
  missing_indicators <- merge(missing_indicators, original_data, by = unit_id)
  
  # Get all unique variable names from missing_ind
  variable_names <- unique(missing$variable)
  
  # Create missingness indicators for each variable
  for (var in variable_names) {
    missing_indicators[[paste0(var, "_miss")]] <- ifelse(
      paste(missing_indicators[[unit_id]], 
            missing_indicators$iter) %in% paste(missing[[unit_id]][missing$variable == var], 
                                                missing$iter[missing$variable == var]), 1, 0)
  }
  
  missing_indicators <- missing_indicators[order(missing_indicators$iter, missing_indicators[[unit_id]]), ]
  
  return(missing_indicators)
}
