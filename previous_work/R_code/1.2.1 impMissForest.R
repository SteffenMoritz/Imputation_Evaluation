#' Imputes data with missForest and saves output to "imputed_values_mf"
#' @param impInput: data to be imputed. Can use return value of "dataPrepImp"

impMissForest <- function(impInput, module_path, auxVar){
  
  #' Get unique ID, SimID and NR from *complete* NR simulation output file
  inputData <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                        stringsAsFactors = FALSE)
  
  SimID <- inputData$SimID
  ID <- inputData$ID
  NR <- inputData$NR
  
  SimID <- as.data.frame(SimID)
  ID <- as.data.frame(ID)
  NR <- as.data.frame(NR)
  
  #' @param k: number of replicates
  k <- length(impInput)
  
  #' @param n: number of records in original data
  n <- nrow(impInput[[1]])
  
  impOutput <- data.frame()
  
  #' Note: if temp <- missForest(...)
  #'       then temp$ximp is the matrix containing results, and is same size as the input matrix 
  tempImpCombined <- matrix(NA, nrow=k*nrow(impInput[[1]]), ncol=ncol(impInput[[1]]))  
  for (j in 1:k){
    temp <- missForest(impInput[[j]], maxiter = 10)
    tempImpCombined[(n*(j-1)+1):(n*j),] <- temp$ximp
  }
  
  colnames(tempImpCombined)=colnames(temp$ximp)
  tempImpCombined <- as.data.frame(tempImpCombined)
  impOutput <- cbind(SimID, ID, NR, subset(tempImpCombined, select=c(Y)))
#  impOutput <- cbind(SimID, ID, NR, tempImpCombined)
  names(impOutput)[names(impOutput) == "Y"] <- "Y_hat"
  
  write.table(impOutput, paste0(module_path, "/02_Imputation/imputed_values_mf_",auxVar,"_full.csv"), 
              sep=",",row.names=FALSE)
  
  #' Select and print imputed data only (i.e. with NR==1)
  NRloc <- vector()
  for (a in 1:nrow(impOutput)){
    if (impOutput$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  impOutShort <- impOutput[NRloc, -which(names(impOutput) %in% c("NR"))]
  write.table(impOutShort, paste0(module_path,"/02_Imputation/imputed_values_mf_",auxVar,".csv"), 
              sep=",",row.names=FALSE)
  
  
}


