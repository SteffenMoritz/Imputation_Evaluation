library(arrow)
library(here)
library(missMethods)

data <- data.frame(read_parquet(here('simulated_data', 'simulated_complete_data.parq')))

data_x <- data[,1:6]

data_y <- data[,-c(2:6)]

data_1y <- data[,-c(2:15)]

missing_mcar <- delete_MCAR(data_1y, 0.15, "dividend")
head(missing_mcar, 15)

simu_function <- function(i){
  simu_i <- delete_MCAR(data_1y, 0.15, "dividend")
  simu_i$miss_ind <- ifelse(is.na(simu_i$dividend), 1, 0)
  simu_i$trial <- i
  
  return(simu_i)
}

# Timer for simulations
start_time <- Sys.time()  # Start timer

result <- map_dfr(1:1000, simu_function)

end_time <- Sys.time()  # End timer
print(end_time - start_time)

# Less than 2 sec when 1000 times 10K-unit data table
# Almost 0.5GB for the resulting file (10M records)

head(result)
head(table(result$trial, result$miss_ind))


#################################################
# Keep the results of missing outputs by ampute as missing indicators for selected records  
missing_mcar <- delete_MCAR(data_1y, 0.15, "dividend")
sum(is.na(missing_mcar$dividend))
tail(missing_mcar, 25)
missing_ind <- data.frame(business_number = missing_mcar[is.na(missing_mcar$dividend), "business_number"])
head(missing_ind, 15)
dim(missing_ind)

simu_function <- function(i){
  simu_data <- delete_MCAR(data_1y, 0.15, "dividend")
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
# Less than 1 sec when 1000 times 10000-unit data table
# Around 0.2GB for the resulting file (1.5M records)

