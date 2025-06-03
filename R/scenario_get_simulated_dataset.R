#' Get scenario iteration using missingness scenario
#'
#' This function produces a dataset with missing values based on a `missingness_scenario` object.
#'
#' @param missingness_scenario A list containing three elements: `original_data`, `scenario_iteration` and `meta`.
#' @param iter A single value specifying which iteration to retain.
#'
#' @return A data frame with missing values for the selected iteration.
#'
#' @examples
#' missingness_scenario <- list(
#'   original_data = data.frame(ID = c(1, 2, 3, 4), var1 = c(7, 5, 9, 11), var2 = c(13, 21, 7, 15)),
#'   miss_iterations = data.frame(ID = c(1, 1, 2, 1, 2), iter = c(1, 1, 1, 2, 2),
#'                                variable = c('var1', 'var2', 'var1', 'var1', 'var1')),
#'   meta = NULL
#' )
#'
#' scenario_get_iteration(missingness_scenario, 3)
#'
#' @export
scenario_get_iteration <- function(missingness_scenario, iter) {

  if (!is.data.frame(missingness_scenario$original_data)) {
    stop("The 'original_data' element should be a data frame.")
  }
  if (!is.data.frame(missingness_scenario$miss_iterations)) {
    stop("The 'miss_iterations' element should be a data frame.")
  }

  x <- missingness_scenario$original_data
  missing <- missingness_scenario$miss_iterations
  meta <- missingness_scenario$meta
  unit_id <- meta$unit_id
  n <- meta$n

  # Validate the input data
  if (!"iter" %in% names(missing)) stop("Column 'iter' not found in 'missing' data frame.")
  if (!"variable" %in% names(missing)) stop("Column 'variable' not found in missing data frame.")
  if (iter > n) stop("Iteration requested not found in missingness_scenario")

  missing_to_impute_i <- function(i) {

    # Identify revenue columns dynamically
    var_cols <- setdiff(names(x), unit_id)

    x_long <- stack(x[var_cols])
    x_long[[unit_id]] <- rep(x[[unit_id]], times = length(var_cols))

    missing_i <- missing[missing$iter==i, !names(missing) %in% "iter"]

    # Replace matching values with NA
    x_long$values[paste(x_long[[unit_id]], x_long$ind) %in%
                    paste(missing_i[[unit_id]], missing_i$variable)] <- NA

    # Convert back to wide format
    miss_to_imp_i <- reshape(x_long, idvar = unit_id, timevar = "ind", direction = "wide")

    # Fix column names
    names(miss_to_imp_i) <- sub("values.", "", names(miss_to_imp_i))

    miss_to_imp_i$iter <- i

    return(miss_to_imp_i)
  }

  scenario_iteration <- missing_to_impute_i(iter)

  # Reset the row index
  rownames(scenario_iteration) <- NULL

  structure(
    list(
      original_data = x,
      scenario_iteration = scenario_iteration,
      meta = meta

    ),
    class = "scenario_iteration"
  )

}
