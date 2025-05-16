#' @function distPlot - overlay of kernel smoothed density of y and yhat 
#'                      (all records, and imputed records)
#' @function distOverlay - overlay of the density of all simulation runs
#'                         and the original distribution

#' @param module_path directory where simulations are saved (parent folder, eg: D1_RBDS/NR1)
#' @param impMethod rd - random donor
#'                   ri - ratio imputation
#'                   rie - ratio imputation with random error
#'                   nn - nearest neighbour
#'                   mf - missing forest
#' @param auxVar column name of the auxiliary variables 
#'               (single variate; consistent with the imputation output)


distPlot <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,"_full.csv"), 
                      stringsAsFactors = FALSE)
  
  # ----------------------------- plotting all records -----------------------------
  Y <- tempIn1$Y # original records, repeated k times
  Yhat <- tempIn2$Y_hat
  df_y <- data.frame(valueY=c(Y,Yhat), caseY=c(rep("Y",length(Y)),rep("Yhat",length(Yhat))))
  
  denY <- density(Y, from = min(Y,Yhat), to = max(Y,Yhat))
  denYhat <- density(Yhat, from = min(Y,Yhat), to = max(Y,Yhat))
  
  # new "density" class where the y-value is the minimum of Yimp or Yhatimp
  denYmin <- denY
  for (i in 1:length(denY[[2]])){
    denYmin[[2]][i]=min(denY[[2]][i],denYhat[[2]][i])
  }
  
  df_splineYmin <- as.data.frame(spline(denYmin[[1]],denYmin[[2]]))
  names(df_splineYmin)[names(df_splineYmin) == "x"] <- "valueY"
  
  ggplot(df_y, aes(x=valueY)) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold"),
          legend.position = c(.95,.95), legend.justification = c("right","top")) +
    geom_density(data=subset(df_y, caseY=="Y"), aes(fill="a1"),color=NA)+
    geom_density(data=subset(df_y, caseY=="Yhat"), aes(fill="a2"),color=NA)+
    scale_fill_manual(values=c("a1"="salmon", "a2"="cornflowerblue"),
                      labels=c("Y_imp < Y","Y_imp > Y"), name=NULL) +
    xlab("Y") +
    ylab("Density") +
    geom_area(data=df_splineYmin, aes(x=valueY, y=y),fill="ivory3", color=NA)
  
  ggsave(paste0(module_path,"/04_Outputs/",impMethod,"/density_plot_",impMethod,"_",auxVar,".png"))
  
  # ----------------------- plotting the imputed values only -----------------------
  NRloc <- vector()
  for (a in 1:nrow(tempIn1)){
    if (tempIn1$NR[a]==1) {
      NRloc <- c(NRloc, a)
    }
  }
  
  Yimp <- tempIn1[NRloc,]$Y
  Yhatimp <- tempIn2[NRloc,]$Y_hat
  df_imp <- data.frame(valueY=c(Yimp,Yhatimp), 
                       caseY=c(rep("Y",length(Yimp)), rep("Yhat",length(Yhatimp))))
  
  denYimp <- density(Yimp, from = min(Yimp,Yhatimp), to = max(Yimp,Yhatimp))
  denYhatimp <- density(Yhatimp, from = min(Yimp,Yhatimp), to = max(Yimp,Yhatimp))
  
  # new "density" class where the y-value is the minimum of Yimp or Yhatimp
  denYminimp <- denYimp
  for (i in 1:length(denYminimp[[2]])){
    denYminimp[[2]][i]=min(denYimp[[2]][i],denYhatimp[[2]][i])
  }
  
  df_splineYminimp <- as.data.frame(spline(denYminimp[[1]],denYminimp[[2]]))
  names(df_splineYminimp)[names(df_splineYminimp) == "x"] <- "valueY"
  
  ggplot(df_imp, aes(x=valueY)) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold"), 
          legend.position = c(.95,.95), legend.justification = c("right","top")) +
    geom_density(data=subset(df_imp, caseY=="Y"), aes(fill="a1"),color=NA)+
    geom_density(data=subset(df_imp, caseY=="Yhat"), aes(fill="a2"),color=NA)+
    scale_fill_manual(values=c("a1"="salmon", "a2"="cornflowerblue"), 
                      labels=c("Y_imp < Y","Y_imp > Y"), name=NULL) +
    xlab("Y (imputed)") +
    ylab("Density") +
    geom_area(data=df_splineYminimp, aes(x=valueY, y=y),fill="ivory3", color=NA)
  
  ggsave(paste0(module_path,"/04_Outputs/",impMethod,"/density_plot_imputed_",impMethod,"_",auxVar,".png"))
  
}




distOverlay <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,"_full.csv"), 
                      stringsAsFactors = FALSE)
  
  n <- count(tempIn1$SimID==1)$freq[2]   # number of records in original data
  k <- nrow(tempIn1)/n  # number of simulations 
  
  Y <- tempIn1[1:n, ]$Y
  df_Y <- data.frame(Y=Y) # original records without repeat
  
  df_Yhat <- list() # list of dataframes containing the j-th imputed records
  for (j in 1:k){
    Yhat <- tempIn2[(n*(j-1)+1):(n*j), ]$Y_hat
    df_Yhat[[j]] <- data.frame(Y=Yhat)
  }
  
  # Yhat <- tempIn2$Y_hat
  # df_Yhat <- data.frame(tempIn2$Y_hat)
  # names(df_Yhat)[names(df_Yhat) == "tempIn2$Y_hat"] <- "Y"
  
  pDist <- ggplot(df_Y) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank()) 
  
  for (j in 1:k){
    pDist <- pDist + geom_line(stat="density", data=df_Yhat[[j]], aes(x=Y), color=rgb(0.6,0.8,1,0.4))
  }
  
  pDist <- pDist +  stat_density(geom="line", aes(x=Y), color= "gray40", linetype="dashed", size=1)
  
  ggsave(plot = pDist, 
         filename = paste0(module_path,"/04_Outputs/",impMethod,"/density_overlay_",impMethod,"_",auxVar,".png"))
  
} 
