#' If the data set does not contain unique identifiers to each record, user can use this program to add unique IDs
#' The function returns a numeric vector of length "length(y)"

#' @param y vector or dataframe containing the variable(s) to be imputed

uniqueID <- function(y) {
  temp <- sprintf("%05d",1:length(y))
  id <- paste("R",temp,sep="")
  return (id)
}
