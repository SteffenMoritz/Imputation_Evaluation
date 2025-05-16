#' @function residualErrorPlotHist - scatter plot of residual errors e_ij (yhat-y) vs y 
#'                                  and relative residual errors e_ij/y vs y (imputed records only)
#' @function avgErrorPlotHist - scatter plot of average errors bar{e_i} vs y 
#'                             and relative average errors bar{e_i}/y vs y (imputed records only)
#' @function residualErrorLogPlotHist - scatter plot of residual errors e_ij (yhat-y) vs log(y)
#'                                  and relative residual errors e_ij/y vs log(y) (imputed records only)
#' @function avgErrorLogPlotHist - scatter plot of average errors bar{e_i} vs log(y) 
#'                             and relative average errors bar{e_i}/y vs log(y) (imputed records only)
#' @function overlayResidualPlot - overlay of (absolute) residual with average residuals 
#'                                     vs y
#' @function rankOverlayResidualPlot - overlay of (absolute) residual with average residuals 
#'                                     vs rank of y
#'                                                            
#' @param module_path directory where simulations are saved (parent folder, eg: D1_RBDS/NR1)
#' @param imp_method rd - random donor
#'                   ri - ratio imputation
#'                   rie - ratio imputation with random error
#'                   nn - nearest neighbour
#'                   mf - missing forest
#' @param auxVar column name of the auxiliary variables 
#'               (single variate; consistent with the imputation output)


residualErrorPlotHist <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read_sas(paste0(module_path, "/01_NonResponse/nr_simulation.sas7bdat"))
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
 
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn3$Y_hat
  
  RE <- Yhatimp - Yimp
  RRE <- (Yhatimp - Yimp)/Yimp
  df_re <- data.frame(RE=RE, RRE=RRE, Y=Yimp)
  limRE <- max(max(RE),-min(RE))
  limRRE <- max(max(RRE),-min(RRE))
  
  # with marginal histogram
  pe1 <- ggplot(df_re, aes(Y,RE)) +
    geom_point(color="dodgerblue3", alpha=min(max(0.1,500/nrow(tempIn3)),0.4)) +
    ylab("Residual Errors") +
    ylim(-limRE, limRE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8) 
  pe1x <- ggExtra::ggMarginal(pe1, type="histogram", color="dodgerblue2", fill="dodgerblue2", alpha=0.2)

  ggsave(plot = pe1x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/residual_error_imputed_",impMethod,"_",auxVar,"_hist.png"))
  
  pe2 <- ggplot(df_re, aes(Y,RRE)) +
    geom_point(color="dodgerblue3", alpha=min(max(0.1,500/nrow(tempIn3)),0.4)) +
    ylab("Relative Residual Errors") +
    ylim(-limRRE, limRRE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pe2x <- ggExtra::ggMarginal(pe2, type="histogram", color="dodgerblue2", fill="dodgerblue2", alpha=0.2)
  
  ggsave(plot = pe2x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/relative_residual_error_imputed_",impMethod,"_",auxVar,"_hist.png"))
  
}



avgErrorPlotHist <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read_sas(paste0(module_path, "/01_NonResponse/nr_simulation.sas7bdat"))
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn3$Y_hat
  RE <- Yhatimp - Yimp
  
  IDimp <- as.factor(tempIn3$ID)
  numericIDimp <- as.numeric(IDimp)
  
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
  
  originalY <- tempIn1[1:max(numericIDimp),]$Y
  AE <- sumRE/sumNR
  RAE <- AE/originalY
  
  df_ae <- data.frame(AE=AE, RAE=RAE, Y=originalY)
  limAE <- max(max(AE),-min(AE))
  limRAE <- max(max(RAE),-min(RAE))
  
  pa1 <- ggplot(df_ae, aes(Y,AE)) +
    geom_point(color="dodgerblue3", alpha=min(max(0.1,500/nrow(df_ae)),0.4)) +
    ylab("Average Errors")+
    ylim(-limAE, limAE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pa1x <- ggExtra::ggMarginal(pa1, type="histogram", color="dodgerblue2", fill="dodgerblue2", alpha=0.2)
  
  ggsave(plot = pa1x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/average_error_imputed_",impMethod,"_",auxVar,"_hist.png"))
  
  pa2 <- ggplot(df_ae, aes(Y,RAE)) +
    geom_point(color="dodgerblue3", alpha=min(max(0.1,500/nrow(df_ae)),0.4)) +
    ylab("Relative Average Errors")+
    ylim(-limRAE, limRAE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pa2x <- ggExtra::ggMarginal(pa2, type="histogram", color="dodgerblue2", fill="dodgerblue2", alpha=0.2)
  
  ggsave(plot = pa2x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/relative_average_error_imputed_",impMethod,"_",auxVar,"_hist.png"))
  
}




residualErrorLogPlotHist <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read_sas(paste0(module_path, "/01_NonResponse/nr_simulation.sas7bdat"))
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  logY <- log(Yimp)
  Yhatimp <- tempIn3$Y_hat
  
  RE <- Yhatimp - Yimp
  RRE <- (Yhatimp - Yimp)/Yimp
  df_re <- data.frame(RE=RE, RRE=RRE, logY=logY)
  limRE <- max(max(RE),-min(RE))
  limRRE <- max(max(RRE),-min(RRE))
  
  pel1 <- ggplot(df_re, aes(logY,RE)) +
    geom_point(color="darkslateblue", alpha=min(max(0.1,500/nrow(tempIn3)),0.4)) +
    ylab("Residual Errors") +
    xlab("log(Y)")+
    ylim(-limRE, limRE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pel1x <- ggExtra::ggMarginal(pel1, type="histogram", color="darkslateblue", fill="darkslateblue", alpha=0.2)
  
  ggsave(plot = pel1x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/residual_error_imputed_log_",impMethod,"_",auxVar,"_hist.png"))
  
  pel2 <- ggplot(df_re, aes(logY,RRE)) +
    geom_point(color="darkslateblue", alpha=min(max(0.1,500/nrow(tempIn3)),0.4)) +
    ylab("Relative Residual Errors")+
    xlab("log(Y)")+
    ylim(-limRRE, limRRE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pel2x <- ggExtra::ggMarginal(pel2, type="histogram", color="darkslateblue", fill="darkslateblue", alpha=0.2)
  
  ggsave(plot = pel2x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/relative_residual_error_imputed_log_",impMethod,"_",auxVar,"_hist.png"))
  
}




avgErrorLogPlotHist <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read_sas(paste0(module_path, "/01_NonResponse/nr_simulation.sas7bdat"))
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
  
  # plotting the imputed values only
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn3$Y_hat
  RE <- Yhatimp - Yimp
  
  IDimp <- as.factor(tempIn3$ID)
  numericIDimp <- as.numeric(IDimp)
  
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
  
  originalY <- tempIn1[1:max(numericIDimp),]$Y
  logY <- log(originalY)
  AE <- sumRE/sumNR
  RAE <- AE/originalY
  
  df_ae <- data.frame(AE=AE, RAE=RAE, logY=logY)
  limAE <- max(max(AE),-min(AE))
  limRAE <- max(max(RAE),-min(RAE))
  
  pal1 <- ggplot(df_ae, aes(logY,AE)) +
    geom_point(color="darkslateblue", alpha=min(max(0.1,500/nrow(df_ae)),0.4)) +
    ylab("Average Errors") +
    xlab("log(Y)")+
    ylim(-limAE, limAE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pal1x <- ggExtra::ggMarginal(pal1, type="histogram", color="darkslateblue", fill="darkslateblue", alpha=0.2)
  
  ggsave(plot = pal1x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/average_error_imputed_log_",impMethod,"_",auxVar,"_hist.png"))
  
  pal2 <- ggplot(df_ae, aes(logY,RAE)) +
    geom_point(color="darkslateblue", alpha=min(max(0.1,500/nrow(df_ae)),0.4)) +
    ylab("Relative Average Errors") +
    xlab("log(Y)")+
    ylim(-limRAE, limRAE) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8)
  pal2x <- ggExtra::ggMarginal(pal2, type="histogram", color="darkslateblue", fill="darkslateblue", alpha=0.2)
  
  ggsave(plot = pal2x, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/relative_average_error_imputed_log_",impMethod,"_",auxVar,"_hist.png"))
  
}




overlayResidualPlot <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"),
                      stringsAsFactors = FALSE)
  
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
  
  n <- count(tempIn1$SimID==1)$freq[2]   # number of records in original data
  k <- nrow(tempIn1)/n  # number of simulations 
  
  copyY <- tempIn1[1:n,]$Y
  
  # imputed records
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  # (absolute) residual errors
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn3$Y_hat
  RE <- Yhatimp - Yimp
  limRE <- max(max(RE),-min(RE))
  df_re <- data.frame(Y=Yimp, RE=RE)
  
  # average residual errors
  IDimp <- as.factor(tempIn1[NRloc,]$ID) 
  numericIDimp <- as.numeric(IDimp)
  
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
  df_ae <- data.frame(Y=copyY, AE=AE)
  
  pe <- ggplot(df_re, aes(Y,RE)) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), 
          plot.title = element_text(hjust=0.5, face = "bold"),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8) +
    geom_point(aes(color="a1"), alpha=min(max(0.1,500/nrow(df_re)),0.4)) +
    geom_point(data=df_ae, aes(Y,AE, color="a2"), alpha=min(max(0.1,500/nrow(df_ae)),0.6)) +
    ylab("Residual Errors") +
    xlab("Y") +
    ylim(-limRE, limRE) +
    scale_color_manual(values=c("a1"="dodgerblue3", "a2"="darkgoldenrod1"), 
                       labels=c("Residual Errors", "Average Errors"),
                       name=NULL) 
  
  ggsave(plot = pe, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/overlay_average_error_",impMethod,"_",auxVar,".png"))
  
}





rankOverlayResidualPlot <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"),
                      stringsAsFactors = FALSE)
  
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
  
  n <- count(tempIn1$SimID==1)$freq[2]   # number of records in original data
  k <- nrow(tempIn1)/n  # number of simulations 
  
  # rank according to Y value
  copyID <- tempIn1[1:n,]$ID
  copyY <- tempIn1[1:n,]$Y
  rankY <- order(copyY)
  df_rank <- data.frame(ID=copyID, rankY=rankY)
  
  # imputed records
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  # (absolute) residual errors
  IDimp <- tempIn1[NRloc,]$ID
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn3$Y_hat
  RE <- Yhatimp - Yimp
  limRE <- max(max(RE),-min(RE))
  df_re <- data.frame(ID=IDimp, Y=Yimp, RE=RE)
  
  df_rerank <- join(df_rank, df_re, by="ID", type="right")
  
  
  # average residual errors
  IDimp <- as.factor(IDimp) # already defined above
  numericIDimp <- as.numeric(IDimp)
  
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
  df_ae <- data.frame(ID=copyID, Y=copyY, AE=AE)
  
  df_aerank <- join(df_rank, df_ae, by="ID", type="right")
  
  
  perank <- ggplot(df_rerank, aes(rankY,RE)) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), 
          plot.title = element_text(hjust=0.5, face = "bold"),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8) +
    geom_point(aes(color="a1"), alpha=min(max(0.1,500/nrow(df_rerank)),0.4)) +
    geom_point(data=df_aerank, aes(rankY,AE, color="a2"), alpha=min(max(0.1,500/nrow(df_aerank)),0.6)) +
    ylab("Residual Errors") +
    xlab("Rank of Y \n (low to high)") +
    ylim(-limRE, limRE) +
    scale_color_manual(values=c("a1"="dodgerblue3", "a2"="darkgoldenrod1"), 
                       labels=c("Residual Errors", "Average Errors"),
                       name=NULL) 
  
  # perankx <- ggExtra::ggMarginal(perank, type="histogram", color="dodgerblue2", fill="dodgerblue2", alpha=0.2)
  ggsave(plot = perank, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/overlay_average_error_rank_",impMethod,"_",auxVar,".png"))
  
  # ggsave(plot = perankx, 
  #        filename = paste0(module_path,"/04_Outputs/",impMethod,"/overlay_average_error_rank_",impMethod,"_",auxVar,".png"))
  
}