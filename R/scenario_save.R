#' Save missingness_scenario as Parquet file
#'
#' This function extracts the `miss_iterations` element from a `missingness_scenario` object
#' then saves it as a Parquet file at the specified location.
#'
#' @param missingness_scenario A list containing two elements: `meta` and `miss_iterations`.
#' @param save_path A string specifying the file path to save the Parquet file.
#' @param optimized A logical indicating if optimized version of specifying of `miss_iterations` is desired.
#'
#' @return None. Saves the `miss_iterations` data frame as a Parquet file.
#'
#' @importFrom arrow write_parquet
#' @examples
#' library(arrow)
#' missingness_scenario <- list(meta = NULL,
#'                              miss_iterations = data.frame(ID = c(1, 2, 3), value = c(NA, 5, 8)))
#' scenario_save(missingness_scenario, "miss_scenario.parquet")
#'
#' @export
scenario_save <- function(missingness_scenario, save_path, optimized=FALSE) {

  # Extract the miss_iterations element
  miss_iterations <- missingness_scenario$miss_iterations

  # Validate the extracted data
  if (!is.data.frame(miss_iterations)) {
    stop("The 'miss_iterations' element should be a data frame.")
  }

  if (optimized){
    # Save the data frame as a Parquet file
    arrow::write_parquet(miss_iterations, save_path)

    message("File saved successfully at: ", save_path)

  } else {

    n <- missingness_scenario$meta$n

    iterations <- do.call(rbind,
                          lapply(seq_len(n),
                                 function(l) scenario_get_iteration(missingness_scenario, l)$scenario_iteration))

    arrow::write_parquet(iterations, save_path)

    message("One iteration file saved successfully at: ", save_path)
  }

}
