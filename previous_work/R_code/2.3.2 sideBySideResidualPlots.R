
#' ! Must call an appropriate plotting function inside the loop
#' Currently does not support to plot with consistent axis (lattice plots) 
#'    as it is not directly supported by ggplot2 and requires manual set-up
#'    cf. "Example3.R" for an example of manually setting up the lattice plots in:
#'    \\fld6filer\esmd-dmse\GenSys\Research\StatisticalDataEditing\Simulation-based Assessment\Simulations\D1_RBDS\Analysis\JSM Presentation\Example 3

# sideBySideResidualPlot <- function(module_path, impMethods, auxVar){
  
  residualPlots <- list(NA)
  
  for (i in 1:length(impMethods)){
    residualPlots[[i]] <- overlayResidualOut(module_path, impMethods[i], auxVar)
  }
  
  tempPlot <- plot_grid(plotlist = residualPlots)
  tempLegend <- get_legend(residualPlots[[1]] + theme(legend.position = "bottom"))
  plot_grid(tempPlot, tempLegend, ncol = 1, rel_heights = c(1,.1))
  
# }




