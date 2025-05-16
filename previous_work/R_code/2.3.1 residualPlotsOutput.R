#' Similar residual plots with 2.3
#' Plots as return values of the function, instead of being written to files
#' Slightly different legends and labels, optimized for overlay
#' 
#' overlayResidualOut ~ overlayResidualPlot
#' 
#' Other plots can be added in a similar way 




overlayResidualOut <- function(module_path, impMethod, auxVar){
  
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
          panel.grid.major.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
    # theme(legend.key = element_blank(),
    #       plot.title = element_text(hjust=0.5, face = "bold"),
    #       panel.grid.major.x = element_blank(),
    #       axis.text.x = element_blank(), axis.ticks = element_blank()) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8) +
    geom_point(aes(color="a1"), alpha=min(max(0.1,500/nrow(df_re)),0.4)) +
    geom_point(data=df_ae, aes(Y,AE, color="a2"), alpha=min(max(0.1,500/nrow(df_ae)),0.6)) +
    # ylab(paste("Residual Errors \n", toupper(impMethod))) +
    ylab(toupper(impMethod)) +
    xlab("Y") +
    ylim(-limRE, limRE) +
    scale_color_manual(values=c("a1"="dodgerblue3", "a2"="darkgoldenrod1"), 
                       labels=c("Residual Errors", "Average Errors"),
                       name=NULL) 
  
  return (pe)
  

}