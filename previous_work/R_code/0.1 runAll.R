# -- Package installation --
# install.packages( pkgs = 'haven',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'missForest',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'plyr',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'dplyr',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'ggplot2',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'ggforce',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'ggExtra',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")
# install.packages( pkgs = 'cowplot',  dependencies = TRUE,
#   repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN")


library(haven) # Read from SAS export file
library(missForest) # Missing forest imputation package
library(plyr) # count, join functions etc.
library(dplyr) # upper-lower case conversion etc.
library(ggplot2)
library(ggforce) # jipper plot etc.
library(ggExtra) # Marginal histogram etc.
library(cowplot) # Arrange multiple plots


# -- Simulating and imputing data --

# If input data does not contain unique identifiers
  ID <- uniqueID(y)
# else continue

# If having "NR_Simulations" file, skip this block and continue to dataPrepImp (line 17)
  # else 
    # If no available simulation patterns ("NR_Indicators_Full" or "NR_Indicators"), create by
      NRsimPoisson(ID, p, k, module_path) # Poisson MCAR
    # If having "NR_Indicators", convert to "NR_Indicators_Full" by
      NRshortToFull(ID, k, module_path)
  
  # with "NR_Indicators_Full", create "NR_Simulation" by
    NRout(y, x, module_path, auxVar)

impInput <- dataPrepMissForest(module_path)

impMissForest(impInput, module_path, auxVar)



# -- Plotting imputed data --

impMethod <- "mf"

# density

distPlot(module_path, impMethod, auxVar)
distOverlay(module_path, impMethod, auxVar)

# residuals

residualErrorPlotHist(module_path, impMethod, auxVar)
avgErrorPlotHist(module_path, impMethod, auxVar)
residualErrorLogPlotHist(module_path, impMethod, auxVar)
avgErrorLogPlotHist(module_path, impMethod, auxVar)

overlayResidualPlot(module_path, impMethod, auxVar)
rankOverlayResidualPlot(module_path, impMethod, auxVar)

residualVsAuxVar(module_path, impMethod, auxVar)


# estimators

estimatorPlot(module_path, impMethod, auxVar)


