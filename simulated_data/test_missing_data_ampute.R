
library(arrow)
library(here)
library(mice)
library(purrr)

data <- data.frame(read_parquet(here('simulated_data', 'simulated_complete_data.parq')))

data_x <- data[,1:6]

data_y <- data[,-c(2:6)]

data_1y <- data[,-c(2:15)]

data_x_1y <- data[,-c(7:15)]
head(data_x_1y)


# Simulate missing

# patterns: A matrix or data frame of size #patterns by #variables where 0 
# indicates that a variable should have missing values and 1 indicates that a 
# variable should remain complete.

# Create pattern to exclude unit_id from being missing-value generated
# var_to_imp <- matrix(1, nrow = dim(data_1y)[2]-1, ncol = dim(data_1y)[2]-1)
# diag(var_to_imp) <- 0
# var_not_to_imp <- rep(1, nrow = dim(data_1y)[2]-1)

# patt <- cbind(var_not_to_imp, var_to_imp)
# patt

pattern <- matrix(c(1,0), nrow=1)
pattern

# MCAR
missing_mcar <- ampute(data=data_1y, patterns = pattern, prop=.15, mech='MCAR')
head(missing_mcar)
head(missing_mcar$amp, 25)
md.pattern(missing_mcar$amp)

missing_mcar$amp$miss_ind <- ifelse(is.na(missing_mcar$amp$dividend), 1, 0)
sum(missing_mcar$amp$miss_ind)
head(missing_mcar$amp, 25)

merged_mcar <- merge(data_1y, missing_mcar$amp[,-2], by = "business_number", all = TRUE)
head(merged_mcar)

# Stack the results of missing outputs by ampute (ampute()$amp)

simu_function <- function(i){
  simu_i <- ampute(data=data_1y, patterns = pattern, prop=.15, mech='MCAR')$amp
  simu_i$miss_ind <- ifelse(is.na(simu_i$dividend), 1, 0)
  simu_i$trial <- i
  
  return(simu_i)
}

result <- map_dfr(1:5, simu_function)

head(result)
table(result$trial, result$miss_ind)

# Timer for simulations
start_time <- Sys.time()  # Start timer

result <- map_dfr(1:1000, simu_function)

end_time <- Sys.time()  # End timer
print(end_time - start_time)

# Less than 7 sec when 1000 times 10K-unit data table
# Almost 0.5GB for the resulting file (10M records)

head(table(result$trial, result$miss_ind))

#################################################
# Keep the results of missing outputs by ampute as missing indicators for selected records  
missing_mcar <- ampute(data=data_1y, patterns = pattern, prop=.15, mech='MCAR')$amp
sum(is.na(missing_mcar$dividend))
head(missing_mcar, 25)
missing_ind <- data.frame(business_number = missing_mcar[is.na(missing_mcar$dividend), "business_number"])
head(missing_ind, 15)
dim(missing_ind)

simu_function <- function(i){
  simu_data <- ampute(data=data_1y, patterns = pattern, prop=.15, mech='MCAR')$amp
  simu_i <- data.frame(business_number = simu_data[is.na(simu_data$dividend), "business_number"])
  simu_i$trial <- i
  
  return(simu_i)
}

result <- map_dfr(1:5, simu_function)

head(result)
dim(result)
table(result$trial)

# Timer for simulations
start_time <- Sys.time()  # Start timer

result <- map_dfr(1:1000, simu_function)

end_time <- Sys.time()  # End timer
print(end_time - start_time)

head(table(result$trial))
# Less than 5 sec when 1000 times 10000-unit data table
# Around 0.2GB for the resulting file (~1.5M records)
