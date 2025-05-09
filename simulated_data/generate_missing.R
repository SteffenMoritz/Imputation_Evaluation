
library(arrow)
library(here)
library(mice)
library(purrr)
library(tictoc)

data <- data.frame(read_parquet(here('simulated_data', 'simulated_complete_data.parq')))

data_1y_1x <- data[,-c(2:3,5:15)]

head(data_1y_1x)


## Function to generate missing values

# simu_function <- function(i){
#   simu_data <- ampute(data=data_1y, patterns = pattern, prop=.15, mech='MCAR')$amp
#   simu_i <- data.frame(business_number = simu_data[is.na(simu_data$dividend), "business_number"])
#   simu_i$trial <- i
#   
#   return(simu_i)
# }

generate_missing <- function(data, unit_id, n, miss_prop, mech, patterns=NULL, weights=NULL){
  
  # Inner
  one_trial <- function(i){
    gen_miss <- ampute(data=data, prop=miss_prop, mech=mech, patterns = patterns, weights=weights)$amp
    # Creating y_var and miss_ind works when one y variable
    y_var = as.character(names(data)[which(patterns==0)])
    gen_miss$miss_ind <- ifelse(is.na(gen_miss[,patterns==0]), TRUE, FALSE)
    gen_miss$trial <- i
    head(gen_miss)
    
    merged <- merge(data, gen_miss[, c(unit_id, "miss_ind", "trial")], by = unit_id, all = TRUE)
  
    return(merged)
  }
  result <- map_dfr(1:n, one_trial)
  return(result)  
  
}

pattern_y <- matrix(c(1,0), nrow=1)
pattern_y_x <- matrix(c(1,1,0), nrow=1)
weights_y_x <- matrix(c(0,1,1), nrow=1)
weights_y <- matrix(c(0,0,1), nrow=1)

tic()
set.seed(123)
result <- generate_missing(data=data_1y_1x, unit_id='business_number', n=1000, 
                           miss_prop=.05, mech='MNAR', patterns=pattern_y_x, 
                           weights=weights_y)
toc()

tail(result, 35)
head(table(result$miss_ind, result$trial))

# Check its size
utils:::format.object_size(object.size(result), "GB")
str(result)
