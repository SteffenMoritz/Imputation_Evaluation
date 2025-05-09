
library(arrow)
library(here)
library(mice)
library(purrr)
library(tictoc)
library(VIM)

data <- data.frame(read_parquet(here('simulated_data', 'simulated_complete_data.parq')))

data_1y_1x <- data[,-c(2:3,5:15)]

head(data_1y_1x)

generate_missing <- function(data, unit_id, n, miss_prop, mech, patterns=NULL, weights=NULL){
  
  # Inner
  one_trial <- function(i){
    gen_miss <- ampute(data=data, prop=miss_prop, mech=mech, patterns = patterns, weights=weights)$amp
    # Creating y_var and miss_ind works when one y variable
    y_var = as.character(names(data)[which(patterns==0)])
    names(gen_miss)[names(gen_miss) == y_var] <- paste0(y_var, "_miss")
    gen_miss$miss_ind <- ifelse(is.na(gen_miss[,patterns==0]), TRUE, FALSE)
    gen_miss$trial <- i

    merged <- merge(data, gen_miss[, c(unit_id, paste0(y_var, "_miss"), "miss_ind", "trial")], by = unit_id, all = TRUE)
    
    return(merged)
  }
  result <- map_dfr(1:n, one_trial)
  return(result)  
  
}

pattern_y_x <- matrix(c(1,1,0), nrow=1)
weights_x <- matrix(c(0,1,0), nrow=1)

tic()
set.seed(123)
result <- generate_missing(data=data_1y_1x, unit_id='business_number', n=1000, 
                           miss_prop=.05, mech='MAR', patterns=pattern_y_x, 
                           weights=weights_x)
toc()


tail(result, 25)


impute_knn <- function(data, variable, k=5, dist_var, imp_var=FALSE){
  
  # Inner
  one_trial <- function(i){
    imputed <- kNN(data=data[data$trial==i,], variable=variable, k=k, dist_var=dist_var, 
                      imp_var=imp_var)
    return(imputed)
  }

  imputed_knn <- map_dfr(1:max(result$trial), one_trial)
  return(imputed_knn)   
  
}


tic()
set.seed(123)
result_imp <- impute_knn(data=result, variable=c("dividend_miss"), k=5, dist_var=c("employees"))
toc()
# It takes ~36min to impute the whole file
