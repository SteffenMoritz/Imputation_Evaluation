#' @function estimatorPlot - jitter plot of simulated mean/total/variance/standard deviation
#'                           with side error bars of SD and RMSE
#'                           
#' @param module_path directory where simulations are saved (parent folder, eg: D1_RBDS/NR1)
#' @param imp_method rd - random donor
#'                   ri - ratio imputation
#'                   rie - ratio imputation with random error
#'                   nn - nearest neighbour
#'                   mf - missing forest
#' @param auxVar column name of the auxiliary variables 
#'               (single variate; consistent with the imputation output)



estimatorPlot <- function(module_path, impMethod, auxVar){
  
  tempIn1 <- read.csv(paste0(module_path, "/01_NonResponse/nr_simulation_",auxVar,".csv"), 
                      stringsAsFactors = FALSE)
  tempIn2 <- read.csv(paste0(module_path, "/02_Imputation/imputed_values_",impMethod,"_",auxVar,"_full.csv"), 
                      stringsAsFactors = FALSE)
  
  Y <- tempIn1$Y
  Yhat <- tempIn2$Y_hat
  
  n <- count(tempIn1$SimID==1)$freq[2]   # number of records in original data
  k <- nrow(tempIn2)/n  # number of simulations 
  
  meanY <- mean(Y[1:n]) # mean of original dataset
  totalY <- sum(Y[1:n])
  varY <- var(Y[1:n])
  sdY <- sd(Y[1:n])
  
  # vector where j-th component is the mean/sum/variance/standard deviation 
  #    of imputed dataset in j-th simulation
  meanYimp <- vector()  
  totalYimp <- vector()  
  varYimp <- vector()  
  sdYimp <- vector()  
  
  for (j in 1:k){
    meanYimp[j] <- mean(Yhat[((j-1)*n+1):(j*n)])
    totalYimp[j] <- sum(Yhat[((j-1)*n+1):(j*n)])
    varYimp[j] <- var(Yhat[((j-1)*n+1):(j*n)])
    sdYimp[j] <- sd(Yhat[((j-1)*n+1):(j*n)])
  }
  
  x <- rep(0,k) # dummy x-axis for locating the jitter plot
  df_est <- data.frame(x=x, ymean=meanYimp, ytotal=totalYimp, yvar=varYimp, ysd=sdYimp)

  # dummy dataframe for plotting the blue/red ribbon of sample bias
  dummyx <- 1
  dummyy <- 1
  df_dummy <- data.frame(dummyx=dummyx, dummyy=dummyy)   
  
  RMSEymean <- sqrt(sum((meanYimp-meanY)^2)/k)
  RMSEytotal <- sqrt(sum((totalYimp-totalY)^2)/k)
  RMSEyvar <- sqrt(sum((varYimp-varY)^2)/k)
  RMSEysd <- sqrt(sum((sdYimp-sdY)^2)/k)
  
  
  
  # --------------------- mean ----------------------------
  ggplot(df_est) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold"),
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    ggtitle("Sample mean of imputed datasets") +
    ylab("Sample mean") +
    xlab(NULL) +
    xlim(-1.5,1.5) +
    ylim(0, 1.2*meanY) + # --- blue (positive) / red (negative) ribbon of sample bias ---
    geom_rect(data = df_dummy, 
              aes(xmin = -Inf, xmax = Inf, ymax = max(meanY,mean(df_est$ymean)), 
                  ymin = min(meanY,mean(df_est$ymean)), fill = "b2"), 
              color = "transparent", alpha = 0.4) +
    geom_hline(aes(yintercept = meanY, linetype = "c1"), color = "gray40", size = 0.8) +
    geom_boxplot(width=0.3, aes(x=x, y=ymean, fill="b1"), color="chartreuse3", 
                 lwd = 0.8, fatten = 1, alpha = 0.2, show.legend = F) + # --- the error bar of sd ---
    geom_errorbar(aes(x = 0.8, ymax = mean(df_est$ymean) + sd(df_est$ymean),
                      ymin = mean(df_est$ymean) - sd(df_est$ymean)),
                  width = 0.1, size = 1, color = "forestgreen") +
    annotate("pointrange", x = 0.8, y = mean(df_est$ymean),
             ymax = mean(df_est$ymean) + sd(df_est$ymean), 
             ymin = mean(df_est$ymean) - sd(df_est$ymean),
             size = 0.7, color = "forestgreen") +
    annotate(geom = "text", x = 0.85, y = mean(df_est$ymean), hjust = 0,
             label = paste("SD of \n sample mean = \n",sd(df_est$ymean)), 
             fontface = "bold", color = "forestgreen") + # --- the error bar of RMSE ---
    geom_errorbar(aes(x = -0.8, ymax = meanY + RMSEymean, ymin = meanY - RMSEymean),
                  width = 0.1, size = 1, color = "forestgreen") +
    annotate("pointrange", x = -0.8, y = meanY,
             ymax = meanY + RMSEymean, ymin = meanY - RMSEymean,
             size = 0.7, color = "forestgreen") +
    annotate(geom = "text", x = -0.85, y = meanY, hjust = 1,
             label = paste("RMSE of \n sample mean = \n",RMSEymean), 
             fontface = "bold", color = "forestgreen") + # --- jitter plot of data points ---
    geom_point(position = position_jitternormal(sd_x=0.15, sd_y=0), aes(x=x, y=ymean, color="a1"), 
               shape=17) +
    scale_fill_manual(values = c("b1"="chartreuse3", 
                                 "b2"=ifelse(mean(df_est$ymean)>meanY,"skyblue1","salmon1"), 
                                 "b3"="salmon"), 
                      labels = c("Percentile of imputed mean", "Sample bias"), name=NULL,
                      guide = guide_legend(override.aes = list(alpha=1), order=3)) +
    scale_color_manual(values = c("a1"="forestgreen"), labels = c("Mean of imputed datasets"),
                       guide = guide_legend(order=2), name=NULL) +
    scale_linetype_manual(values = c("c1"=2), labels = "True mean", 
                          guide = guide_legend(order=1), name = NULL) +
    geom_hline(yintercept = 0, color = "transparent")
  ggsave(paste0(module_path,"/04_Outputs/",impMethod,"/sample_mean_",impMethod,"_",auxVar,".png"))
  
  
  
  # --------------------- total ----------------------------
  ggplot(df_est) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold"),
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    ggtitle("Sample total of imputed datasets") +
    ylab("Sample total") +
    xlab(NULL) +
    xlim(-1.5,1.5) +
    ylim(0, 1.2*totalY) + # --- blue (positive) / red (negative) ribbon of sample bias ---
    geom_rect(data=df_dummy,
              aes(xmin=-Inf, xmax=Inf, ymax=max(totalY,mean(df_est$ytotal)), 
                  ymin=min(totalY,mean(df_est$ytotal)), fill="b2"), 
              color="transparent", alpha=0.4) +
    geom_hline(aes(yintercept=totalY, linetype="c1"), color="gray40", size=0.8) +
    geom_boxplot(width=0.3, aes(x=x, y=ytotal, fill="b1"), color="chartreuse3", 
                 lwd=0.8, fatten=1, alpha=0.2, show.legend = F) + # --- the error bar of sd ---
    geom_errorbar(aes(x=0.8, ymax=mean(df_est$ytotal)+sd(df_est$ytotal),
                      ymin=mean(df_est$ytotal)-sd(df_est$ytotal)),
                  width=0.1, size=1, color="forestgreen") +
    annotate("pointrange", x=0.8, y=mean(df_est$ytotal),
             ymax=mean(df_est$ytotal)+sd(df_est$ytotal), 
             ymin=mean(df_est$ytotal)-sd(df_est$ytotal),
             size=0.7, color="forestgreen") +
    annotate(geom="text", x=0.85, y=mean(df_est$ytotal), hjust=0,
             label=paste("SD of \n sample total = \n",sd(df_est$ytotal)), 
             fontface="bold", color="forestgreen") + # --- the error bar of RMSE ---
    geom_errorbar(aes(x=-0.8, ymax=totalY+RMSEytotal, ymin=totalY-RMSEytotal),
                  width=0.1, size=1, color="forestgreen") +
    annotate("pointrange", x=-0.8, y=totalY,
             ymax=totalY+RMSEytotal, ymin=totalY-RMSEytotal,
             size=0.7, color="forestgreen") +
    annotate(geom="text", x=-0.85, y=totalY, hjust=1,
             label=paste("RMSE of \n sample total = \n",RMSEysd), 
             fontface="bold", color="forestgreen") + # --- jitter plot of data points ---
    geom_point(position=position_jitternormal(sd_x=0.15, sd_y=0), aes(x=x, y=ytotal, color="a1"), 
               shape=17) +
    scale_fill_manual(values=c("b1"="chartreuse3", 
                               "b2"=ifelse(mean(df_est$ytotal)>totalY,"skyblue1","salmon1"), 
                               "b3"="salmon"), 
                      labels=c("Percentile of imputed total", "Sample bias"), name=NULL,
                      guide=guide_legend(override.aes = list(alpha=1), order=3)) +
    scale_color_manual(values = c("a1"="forestgreen"), labels=c("Total of imputed datasets"),
                       guide=guide_legend(order=2), name=NULL) +
    scale_linetype_manual(values = c("c1"=2), labels="True total", 
                          guide=guide_legend(order=1), name=NULL) +
    geom_hline(yintercept=0, color="transparent")
  ggsave(paste0(module_path,"/04_Outputs/",impMethod,"/sample_total_",impMethod,"_",auxVar,".png"))
  
  
  
  # --------------------- variance ----------------------------
  ggplot(df_est) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold"),
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    ggtitle("Sample variance of imputed datasets") +
    ylab("Sample Variance") +
    xlab(NULL) +
    xlim(-1.5,1.5) +
    ylim(0, 1.2*varY) + # --- blue (positive) / red (negative) ribbon of sample bias ---
    geom_rect(data=df_dummy,
              aes(xmin=-Inf, xmax=Inf, ymax=max(varY,mean(df_est$yvar)), 
                  ymin=min(varY,mean(df_est$yvar)), fill="b2"), 
              color="transparent", alpha=0.4) +
    geom_hline(aes(yintercept=varY, linetype="c1"), color="gray40", size=0.8) +
    geom_boxplot(width=0.3, aes(x=x, y=yvar, fill="b1"), color="chartreuse3", 
                 lwd=0.8, fatten=1, alpha=0.2, show.legend = F) + # --- the error bar of sd ---
    geom_errorbar(aes(x=0.8, ymax=mean(df_est$yvar)+sd(df_est$yvar),
                      ymin=mean(df_est$yvar)-sd(df_est$yvar)),
                  width=0.1, size=1, color="forestgreen") +
    annotate("pointrange", x=0.8, y=mean(df_est$yvar),
             ymax=mean(df_est$yvar)+sd(df_est$yvar), 
             ymin=mean(df_est$yvar)-sd(df_est$yvar),
             size=0.7, color="forestgreen") +
    annotate(geom="text", x=0.85, y=mean(df_est$yvar), hjust=0,
             label=paste("SD of \n sample var = \n",sd(df_est$yvar)), 
             fontface="bold", color="forestgreen") + # --- the error bar of RMSE ---
    geom_errorbar(aes(x=-0.8, ymax=varY+RMSEyvar,
                      ymin=varY-RMSEyvar),
                  width=0.1, size=1, color="forestgreen") +
    annotate("pointrange", x=-0.8, y=varY,
             ymax=varY+RMSEyvar, ymin=varY-RMSEyvar,
             size=0.7, color="forestgreen") +
    annotate(geom="text", x=-0.85, y=varY, hjust=1,
             label=paste("RMSE of \n sample var = \n",RMSEyvar), 
             fontface="bold", color="forestgreen") + # --- jitter plot of data points ---
    geom_point(position=position_jitternormal(sd_x=0.15, sd_y=0), aes(x=x, y=yvar, color="a1"), 
              shape=17) +
    scale_fill_manual(values=c("b1"="chartreuse3", 
                               "b2"=ifelse(mean(df_est$yvar)>varY,"skyblue1","salmon1"), 
                               "b3"="salmon"), 
                      labels=c("Percentile of imputed variance", "Sample bias"), name=NULL,
                      guide=guide_legend(override.aes = list(alpha=1), order=3)) +
    scale_color_manual(values = c("a1"="forestgreen"), labels=c("Variance of imputed datasets"),
                       name=NULL, guide=guide_legend(order=2)) +
    scale_linetype_manual(values = c("c1"=2), labels="True variance", name=NULL, 
                          guide=guide_legend(order=1)) +
    geom_hline(yintercept=0, color="transparent")
  ggsave(paste0(module_path,"/04_Outputs/",impMethod,"/sample_variance_",impMethod,"_",auxVar,".png"))  
  
  
  
  # --------------------- standard deviation ----------------------------
  ggplot(df_est) +
    theme_set(theme_bw()) +
    theme(legend.key = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust=0.5, face = "bold"),
          axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
    ggtitle("Sample standard deviation of imputed datasets") +
    ylab("Sample standard deviation") +
    xlab(NULL) +
    xlim(-1.5,1.5) +
    ylim(0, 1.2*sdY) + # --- blue (positive) / red (negative) ribbon of sample bias ---
    geom_rect(data=df_dummy,
              aes(xmin=-Inf, xmax=Inf, ymax=max(sdY,mean(df_est$ysd)), 
                  ymin=min(sdY,mean(df_est$ysd)), fill="b2"), 
              color="transparent", alpha=0.4) +
    geom_hline(aes(yintercept=sdY, linetype="c1"), color="gray40", size=0.8) +
    geom_boxplot(width=0.3, aes(x=x, y=ysd, fill="b1"), color="chartreuse3", 
                 lwd=0.8, fatten=1, alpha=0.2, show.legend = F) + # --- the error bar of sd ---
    geom_errorbar(aes(x=0.8, ymax=mean(df_est$ysd)+sd(df_est$ysd),
                      ymin=mean(df_est$ysd)-sd(df_est$ysd)),
                  width=0.1, size=1, color="forestgreen") +
    annotate("pointrange", x=0.8, y=mean(df_est$ysd),
             ymax=mean(df_est$ysd)+sd(df_est$ysd), ymin=mean(df_est$ysd)-sd(df_est$ysd),
             size=0.7, color="forestgreen") +
    annotate(geom="text", x=0.85, y=mean(df_est$ysd), hjust=0,
             label=paste("SD of \n sample sd = \n",sd(df_est$ysd)), 
             fontface="bold", color="forestgreen") + # --- the error bar of RMSE ---
    geom_errorbar(aes(x=-0.8, ymax=sdY+RMSEysd,
                      ymin=sdY-RMSEysd),
                  width=0.1, size=1, color="forestgreen") +
    annotate("pointrange", x=-0.8, y=sdY,
             ymax=sdY+RMSEysd, ymin=sdY-RMSEysd,
             size=0.7, color="forestgreen") +
    annotate(geom="text", x=-0.85, y=sdY, hjust=1,
             label=paste("RMSE of \n sample sd = \n",RMSEysd), 
             fontface="bold", color="forestgreen") + # --- jitter plot of data points ---
    geom_point(position=position_jitternormal(sd_x=0.15, sd_y=0), aes(x=x, y=ysd, color="a1"), 
               shape=17) +
    scale_fill_manual(values=c("b1"="chartreuse3", 
                               "b2"=ifelse(mean(df_est$ysd)>sdY,"skyblue1","salmon1"), 
                               "b3"="salmon"), 
                      labels=c("Percentile of imputed SD", "Sample bias"), name=NULL,
                      guide=guide_legend(override.aes = list(alpha=1), order=3)) +
    scale_color_manual(values = c("a1"="forestgreen"), labels=c("SD of imputed datasets"),
                       guide=guide_legend(order=2), name=NULL) +
    scale_linetype_manual(values = c("c1"=2), labels="True standard deviation", 
                          guide=guide_legend(order=1), name=NULL) +
    geom_hline(yintercept=0, color="transparent")
  ggsave(paste0(module_path,"/04_Outputs/",impMethod,"/sample_std_dev_",impMethod,"_",auxVar,".png"))
  
}
