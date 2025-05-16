#' @function histPlot - overlayed histogram of y and yhat
#' @function residualErrorPlotTile - scatter plot of residual errors e_ij (yhat-y) vs y 
#'                                  and relative residual errors e_ij/y vs y (imputed records only)
#' @function avgErrorPlotTile - scatter plot of average errors bar{e_i} vs y 
#'                             and relative average errors bar{e_i}/y vs y (imputed records only)
#' @function residualErrorLogPlotTile - scatter plot of residual errors e_ij (yhat-y) vs log(y)
#'                                  and relative residual errors e_ij/y vs log(y) (imputed records only)
#' @function avgErrorLogPlotTile - scatter plot of average errors bar{e_i} vs log(y) 
#'                             and relative average errors bar{e_i}/y vs log(y) (imputed records only)
#'           All overlaid with tiles (bin2d)


#' Overlay of original dataset Y (red) and imputed dataset Yhat (blue)
#' Needs standard files "Input_Data.csv" and "Imputed_Data.csv"
histPlot <- function(module_path, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_mf_",auxVar,"_full.csv"), 
                      stringsAsFactors = FALSE)
  
  Y <- tempIn1$Y
  Yhat <- tempIn2$Y_hat
  df_hist <- data.frame(valueY=c(Y,Yhat), caseY=c(rep("Y",length(Y)),rep("Yhat",length(Yhat))))
  
  numBins <- 50
  stepSizeHist <- (max(Y,Yhat)-min(Y,Yhat))/numBins
  tempHist1 <- hist(Y, breaks = seq(min(Y,Yhat), max(Y,Yhat), length.out = (numBins+1)))
  tempHist2 <- hist(Yhat, breaks = seq(min(Y,Yhat), max(Y,Yhat), length.out = (numBins+1)))
  tempHist <- vector()
  for (i in 1:numBins){
    tempHist[i] <- min(tempHist1$counts[i], tempHist2$counts[i])
  }
  binLoc <- seq(from=min(Y,Yhat)+stepSizeHist/2, to=max(Y,Yhat)-stepSizeHist/2, by=stepSizeHist)
  df_freq <-data.frame(valueY=binLoc, tempHist) 
  # need to make sure the name of x-axis (binLoc) is consistent with imputation data(valueY) to overlay plots
  
  ggplot(df_hist, aes(x=valueY, fill=caseY)) +
    geom_histogram(data=subset(df_hist, caseY=="Y"), 
                   breaks=seq(min(Y,Yhat), max(Y,Yhat), length.out = (numBins+1))) +
    geom_histogram(data=subset(df_hist, caseY=="Yhat"), 
                   breaks=seq(min(Y,Yhat), max(Y,Yhat), length.out = (numBins+1))) +
    scale_fill_manual(values=c(rgb(1,0,0,0.6), rgb(0,0.4,1,0.6))) +
    theme(legend.title = element_blank()) +
    geom_bar(data=df_freq, aes(y=tempHist, x=valueY), stat="identity", 
             width=stepSizeHist, fill="white", alpha=0.8)
  # legend is darker than plot by default, though adjusted for transparency
  
  ggsave(paste0(module_path,"/04_Outputs/MF/histogram_imputed_mf_",auxVar,".png")) 
  
}





residualErrorPlotTile <- function(module_path, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_mf_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn2$Y_hat
  
  RE <- Yhatimp - Yimp
  RRE <- (Yhatimp - Yimp)/Yimp
  df_re <- data.frame(RE=RE, RRE=RRE, Y=Yimp)
  
  # with "heatmap" overlay
  ggplot(df_re, aes(Y,RE)) +
    geom_point(color="darkorange", alpha=min(500/nrow(tempIn2),0.2)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0,0.5,0.5,0.2),high=rgb(0,0.5,0.5,0.7)) + 
    ylab("Residual Errors")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/residual_error_imputed_mf_",auxVar,"_tile.png"))
  
  # with marginal histogram
  # p<-  ggplot(df_re, aes(Y,RE)) +
  #   geom_point(color="steelblue", alpha=min(500/nrow(tempIn2),0.2)) +
  #   ylab("Residual Errors")
  # ggExtra::ggMarginal(p, type="histogram", color="steelblue", fill="steelblue", alpha=0.6)
  # 
  # ggsave(paste(module_path,"/04_Outputs/MF/residual_error_imputed_mf_",auxVar,"_hist.png",sep=""))
  
  ggplot(df_re, aes(Y,RRE)) +
    geom_point(color="darkorange", alpha=min(500/nrow(tempIn2),0.2)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0,0.5,0.5,0.2),high=rgb(0,0.5,0.5,0.7)) + 
    ylab("Relative Residual Errors")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/relative_residual_error_imputed_mf_",auxVar,"_tile.png"))
  
}



avgErrorPlotTile <- function(module_path, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_mf_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn2$Y_hat
  RE <- Yhatimp - Yimp
  
  IDimp <- as.factor(tempIn2$ID)
  numericIDimp <- as.numeric(IDimp)
  numericID <- seq(from=1,to=max(numericIDimp),by=1)
  # vector where b-th entry is the sum of NR indicators wrt unique ID=b
  sumNR <- vector()
  for (b in 1:max(numericIDimp)){
    sumNR[b] <- count(numericIDimp == b)$freq[2]
  }
  
  df_temp <- data.frame(RE=RE, ID=numericIDimp)
  # vector where b-th entry is the sum of residual errors wrt unique ID=b
  sumRE <- vector()
  for (b in 1:max(numericIDimp)){
    sumRE[b] <- sum(df_temp$RE[df_temp$ID == b])
  }
  
  AE <- sumRE/sumNR
  
  originalY <- tempIn1[1:max(numericIDimp),]$Y
  
  RAE <- AE/originalY
  
  df_ae <- data.frame(AE=AE, RAE=RAE, Y=originalY)
  
  ggplot(df_ae, aes(Y,AE)) +
    geom_point(color="coral1", alpha=min(500/nrow(df_ae),0.5)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0.4,0.4,0.8,0.2),high=rgb(0.4,0.4,0.8,0.7)) +
    ylab("Average Errors")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/average_error_imputed_mf_",auxVar,"_tile.png"))
  
  ggplot(df_ae, aes(Y,RAE)) +
    geom_point(color="coral1", alpha=min(500/nrow(df_ae),0.5)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0.4,0.4,0.8,0.2),high=rgb(0.4,0.4,0.8,0.7)) +
    ylab("Relative Average Errors")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/relative_average_error_imputed_mf_",auxVar,"_tile.png"))
  
}




residualErrorLogPlotTile <- function(module_path, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_mf_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  logY <- log(Yimp)
  Yhatimp <- tempIn2$Y_hat
  
  RE <- Yhatimp - Yimp
  RRE <- (Yhatimp - Yimp)/Yimp
  df_re <- data.frame(RE=RE, RRE=RRE, logY=logY)
  
  ggplot(df_re, aes(logY,RE)) +
    geom_point(color="darkorange", alpha=min(500/nrow(tempIn2),0.2)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0,0.5,0.5,0.2),high=rgb(0,0.5,0.5,0.7)) + 
    ylab("Residual Errors") +
    xlab("log(Y)")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/residual_error_imputed_log_mf_",auxVar,"_tile.png"))
  
  ggplot(df_re, aes(logY,RRE)) +
    geom_point(color="darkorange", alpha=min(500/nrow(tempIn2),0.2)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0,0.5,0.5,0.2),high=rgb(0,0.5,0.5,0.7)) + 
    ylab("Relative Residual Errors")+
    xlab("log(Y)")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/relative_residual_error_imputed_log_mf_",auxVar,"_tile.png"))
  
}




avgErrorLogPlotTile <- function(module_path, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_mf_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn2$Y_hat
  RE <- Yhatimp - Yimp
  
  IDimp <- as.factor(tempIn2$ID)
  numericIDimp <- as.numeric(IDimp)
  numericID <- seq(from=1,to=max(numericIDimp),by=1)
  # vector where i-th entry is the sum of NR indicators wrt unique ID=i
  sumNR <- vector()
  for (b in 1:max(numericIDimp)){
    sumNR[b] <- count(numericIDimp == b)$freq[2]
  }
  
  df_temp <- data.frame(RE=RE, ID=numericIDimp)
  # vector where i-th entry is the sum of residual errors wrt unique ID=i
  sumRE <- vector()
  for (b in 1:max(numericIDimp)){
    sumRE[b] <- sum(df_temp$RE[df_temp$ID == b])
  }
  
  AE <- sumRE/sumNR
  
  originalY <- tempIn1[1:max(numericIDimp),]$Y
  logY <- log(originalY)
  
  RAE <- AE/originalY
  
  df_ae <- data.frame(AE=AE, RAE=RAE, logY=logY)
  
  ggplot(df_ae, aes(logY,AE)) +
    geom_point(color="coral1", alpha=min(500/nrow(df_ae),0.5)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0.4,0.4,0.8,0.2),high=rgb(0.4,0.4,0.8,0.7)) +
    ylab("Average Errors") +
    xlab("log(Y)")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/average_error_imputed_log_mf_",auxVar,"_tile.png"))
  
  ggplot(df_ae, aes(logY,RAE)) +
    geom_point(color="coral1", alpha=min(500/nrow(df_ae),0.5)) +
    geom_bin2d() +
    scale_fill_gradient(low=rgb(0.4,0.4,0.8,0.2),high=rgb(0.4,0.4,0.8,0.7)) +
    ylab("Relative Average Errors") +
    xlab("log(Y)")
  
  ggsave(paste0(module_path,"/04_Outputs/MF/relative_average_error_imputed_log_mf_",auxVar,"_tile.png"))
  
}