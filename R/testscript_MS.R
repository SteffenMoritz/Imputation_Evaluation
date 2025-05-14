library(here)
library(mice)
library(arrow)

source(here("R", "simulate_missing_data.R")) 

data <- data.frame(read_parquet(here('simulated_data', 'simulated_complete_data.parq')))
head(data)

### Function Definitions ####

# Function to generate missing with a package e.g. mice
miss_ampute <- function(data, prop, ...){
  
  missing <- mice::ampute(data=data, prop=prop, ...)$amp
  return(missing)
}

# Function to generate missing with a package e.g. mice
miss_missMethods <- function(data, prop, method='delete_MCAR', ...){
  
  missMethods <- get(method, envir = asNamespace("missMethods"))
  
  missing <- missMethods(ds=data, p=prop, ...)
  
  # missing <- missMethods::delete_MCAR(ds=data, p=prop, ...)
  return(missing)
}

### Procudure ####
#class simulations
md_sims  <- simulate_missing_data(x=data, unit_id='business_number',
                                  miss_func=miss_missMethods, method='delete_MNAR_rank', cols_mis=11, 
                                  n = 100)

tail(md_sims$miss_trials, 25)

save_simulations(md_sims, file =)
get_simulation(number)

print(md_sims)

simulate_missing_data()

# save(obj)
# simulations_save(md_sims, file =)
# simulations_get(class simulation)
# simulations_print
# simulations_plot

