
residualVsAuxVar <- function(module_path, impMethod, auxVar){
  
  # tempIn1 <- read_sas(paste0(module_path, "/01_NonResponse/nr_simulation.sas7bdat"))
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"))
  
  if (impMethod == "rd"){
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,".sas7bdat"))
  } else {
    tempIn3 <- read_sas(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,".sas7bdat"))
  }
  
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  # ideally auxVar should be consistent with column names
  # Xauxvar <- subset(tempIn1[NRloc,], select=c(auxVar))
  
  # if not (need to customize for new input files!)
  # tempchar <- substr(auxVar, 2, nchar(auxVar))
  # auxVarName <- paste0("V_",tempchar)
  # Xauxvar <- subset(tempIn1[NRloc,], select=c(auxVarName))
  # Xauxvar <- as.vector(unlist(Xauxvar))
  
  Xauxvar <- tempIn1[NRloc,]$V1
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn3$Y_hat
  RE <- Yhatimp - Yimp
  
  df_re <- data.frame(RE=RE, X=Xauxvar)
  limRE <- max(max(RE),-min(RE))
  
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
  
  originalX <- tempIn1[1:max(numericIDimp),]$V1
  AE <- sumRE/sumNR
  
  df_ae <- data.frame(AE=AE, X=originalX)
  
  pex <- ggplot(df_re, aes(x=X, y=RE)) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), 
          plot.title = element_text(hjust=0.5, face = "bold"),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    geom_point(aes(color="a1"), alpha=min(max(0.1,500/nrow(tempIn3)),0.4)) +
    geom_point(data=df_ae, aes(X, AE, color="a2"), 
               alpha=min(max(0.1,500/nrow(df_ae)),0.4)) +
    ylab("Residual Errors") +
    xlab(auxVar) +
    ylim(-limRE, limRE) +
    scale_color_manual(values=c("a1"="dodgerblue3", "a2"="coral"), 
                       labels=c("Residual Errors", "Average Errors"),
                       name=NULL) +
    geom_hline(aes(yintercept=0), color="gray40", size=0.8) 
  # pexx <- ggExtra::ggMarginal(pex, type="histogram", color="dodgerblue2", fill="dodgerblue2", alpha=0.2)
  
  ggsave(plot = pex, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/residual_error_imputed_vs_variable_",impMethod,"_",auxVar,"_hist.png"))
  # ggsave(plot = pexx, 
  #        filename = paste0(module_path,"/04_Outputs/",impMethod,"/residual_error_imputed_vs_variable_",impMethod,"_",auxVar,"_hist.png"))
  
}