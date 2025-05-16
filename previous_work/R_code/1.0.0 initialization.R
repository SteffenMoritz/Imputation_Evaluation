
buildingData <- 
  read.csv("//fld6filer/esmd-dmse/GenSys/Research/StatisticalDataEditing/Simulation-based Assessment/Simulations/D1_RBDS/INPUT_DATA/Copy-of-Residential-Building-Data-Set.csv", header = TRUE, skip = 1)


# -- Set up with no given NR patterns --

# Use parent folder (e.g. .../NR1) 
module_path <- 
  "//fld6filer/esmd-dmse/GenSys/Research/StatisticalDataEditing/Simulation-based Assessment/Simulations/D1_RBDS/NR00_test"

y <- buildingData$V.9

auxVar <- "v8" #appended to the file name of "imputed_values"
x <- cbind(buildingData$V.8, buildingData$V.8)

k <- 100
p <- 0.25




# -- Set up with given NR patterns (short file) --