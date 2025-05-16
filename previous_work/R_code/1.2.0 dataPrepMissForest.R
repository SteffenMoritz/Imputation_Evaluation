#' Requires "nr_simulation.csv"
#' *Returns* a list of matrices, one matrix for each simulation
#' elements of the list of matrices are passed into MissForest for imputation
#' (Note: MissForest automatically takes in all variables in the matrix as auxilliary and it cannot be turned off)
#' (so SimID, ID and NR are not kept in the matrix)

dataPrepMissForest <- function(module_path){
  
  simData <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  
  #' @param k: number of replicates
  k <- max(simData$SimID)
  
  #' @param n: number of records in data
  n <- length(simData$SimID)/k
  
  #' Delete value of Y if flagged as missing by NR_indicator
  tempY <- simData$Y
  for(i in 1:nrow(simData)){	
    if(simData$NR[i] == 1){
      tempY[i] <- NA
    }
  }
  simData$Y <- tempY
  
  
  impInput <- list()
  for (j in 1:k){
#   selecting rows of the i-th simulation, and
#   dropping the first 3 columns (SimID, ID and NR) 
    impInput[[j]] <- as.matrix(simData[(n*(j-1)+1):(n*j), 4:ncol(simData)])
  }
  
  return(impInput)
}
