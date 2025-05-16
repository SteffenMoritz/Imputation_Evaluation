#' @function NRsimPoisson:
#' Simulates a Poisson MCAR NR pattern, if NR pattern is not otherwise specified
#' NR indicator - 1:Y 0:N
#' Outputs NR_indicator (both 0's and 1's) to "./NR_Indicators_Full.csv"

#' @function NRout:
#' Takes input from "./NR_Indicators_Full.csv"
#' Appends replicate datasets and output to "./NR_Simulation.csv"

#' @function NRshortToFull:
#' Converts the short "NR_Indicators" (only 1's) file to the full "NR_Indicators_Full"

#' @function NRfullToShort:
#' Converts the full "NR_Indicators_Full" file to the short "NR_Indicators"

#' @param id vector or dataframe containing the unique ID of records 
#'           (can be created by the "uniqueID" function if N/A)
#' @param y vector or dataframe containing the variable(s) to be imputed
#' @param x vecotr or dataframe containing the auxiliary variable(s)
#' @param p vector or dataframe containing the probabilities of NON-RESPONSE
#' @param k number of replicates to create
#' @param module_path directory where simulations will be saved (parent folder, eg: D1_RBDS/NR1)
#' @export
#' @examples
#' NRsim(df,1000,NA,"K:/Census RD/...")

NRsimPoisson <- function(ID, p, k, module_path){
  
  #' number of records in data  
  n <- length(ID)
  
  if (length(p)==1) {
    p <- rep(p, n)
  } else { # do nothing, assuming that p is a vector of correct length
  }
  
  allSimID <- vector(length=n*k)
  allID <- vector(length=n*k)
  allNR <- vector(length=n*k)

  for (j in 1:k) {

    allSimID[(n*(j-1)+1):(n*j)] <- rep(j, n)
    allID[(n*(j-1)+1):(n*j)] <- ID
    
    for (i in 1:n){
      allNR[n*(j-1)+i] <- rbinom(n=1, size=1, p[i])
    }
  }
  df_NR <- data.frame(SimID=allSimID, ID=allID, NR=allNR)
  
  write.table(df_NR, paste0(module_path,"/01_NonResponse/nr_indicators_full.csv"), 
              sep=",",row.names=FALSE)
}




NRout <- function(Y, X, module_path, auxVar){
  
  simData <- read.csv(paste0(module_path, "/01_NonResponse/nr_indicators_full.csv"), 
                      stringsAsFactors = FALSE)
  
  #' number of replicates
  k <- max(simData$SimID)
  
  #' number of records in data
  n <- length(Y)
  
  # Check consistency of record length
  if (n*k == length(simData$SimID)){ # consistent, proceeds to output

    # Adding Y values to dataframe
    tempY <- vector()
    for (j in 1:k){
      for (i in 1:n){
        tempY[n*(j-1)+i] <- Y[i]
      }
    }
    simData$Y <- tempY
    
    # Adding X values to dataframe
    tempX <- matrix(NA, nrow=(n*k), ncol=ncol(X))
    for (j in 1:k){
      tempX[(n*(j-1)+1):(n*j),] <- X
    }
    tempX <- as.data.frame(tempX)
    simData <- cbind(simData, tempX)
    write.table(simData, paste0(module_path,"/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                sep=",",row.names=FALSE)
    
  } else {
    stop('Inconsistent inputs from nr_indicators_full.csv" and data records')
  }
  
}




NRshortToFull <- function(ID, k, module_path){
  
  df_in <- read.csv(paste0(module_path, "/01_NonResponse/nr_indicators.csv"), 
                    stringsAsFactors = FALSE)
  n <- length(ID)
  # "NR_Indicators" contains non-response only (NR=1)
  df_in$NR <- 1
  
  tempSimID <- vector()
  tempID <- vector()
  for (j in 1:k) {
    tempSimID <- c(tempSimID, rep(j, n))
    tempID <- c(tempID, ID)
  }
  
  df_temp1 <- data.frame (SimID=as.character(tempSimID), ID=tempID)
  
  # fill the NR column by 0, and update values of NR by matching SimID & ID with df_in
  df_temp1$NR <- 0
  df_temp2 <- join(df_in, df_temp1, by=c("SimID", "ID"), type="full")
  df_out <- df_temp2[with(df_temp2, order(SimID, ID)),]

  write.table(df_out, paste0(module_path,"/01_NonResponse/nr_indicators_full.csv"), 
              sep=",",row.names=FALSE)
}





NRfullToShort <- function(module_path){
  
  df_in <- read.csv(paste0(module_path, "/01_NonResponse/nr_indicators_full.csv"), 
                    stringsAsFactors = FALSE)
  NRloc <- vector()
    
  for (a in 1:nrow(df_in)){
    if (df_in$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  df_out <- df_in[NRloc, -which(names(df_in) %in% c("NR"))]
  write.table(df_out, paste0(module_path,"/01_NonResponse/nr_indicators.csv"), 
              sep=",",row.names=FALSE)
}
