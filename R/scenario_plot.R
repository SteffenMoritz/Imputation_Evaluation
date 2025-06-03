#' Plot Missingness Scenario
#'
#' This function visualizes the missingness pattern in a dataset using `md.pattern()` from `mice` package.
#'
#' @param x A dataframe that contains missingness.
#' @param iter An optional value that specifies for which iteration the missingness plot is desired.
#'
#' @return A plot displaying the missingness pattern.
#'
#' @importFrom mice md.pattern
#' @examples
#' library(mice)
#' df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA), iter = rep(1, 3))
#' plot_missingness(df)
#'
#' @export
iteration_plot <- function(x, iter=NULL) {
  if (!length(unique(x$iter)) == 1 & is.null(iter)) {
    stop("Data.frame has more than one iteration. Specify an iteration in input.")
  }
  else if (!is.null(iter)){
    x <- x[x$iter == iter,]
  }
  md.pattern(subset(x, select = -iter), rotate.names = TRUE)
}
