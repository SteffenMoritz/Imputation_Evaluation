library(here)
library(mice)
library(arrow)

source(here("R", "a_generate_missingness_scenario.R")) 
source(here("R", "scenario_get_simulated_dataset.R")) 
source(here("R", "scenario_save.R")) 
source(here("R", "scenario_plot.R")) 
source(here("R", "create_missing_indicators.R")) 

data <- data.frame(read_parquet(here('simulated_data', 'simulated_complete_data.parq')))
# No factor columns in data_bis
data_bis <- subset(data, select = -c(employees_cat, small_business))
head(data_bis)

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
#class missingness_scenario
gen_miss  <- a_generate_missingness_scenario(x=data_bis, unit_id='business_number',
                                                 miss_func=miss_missMethods, method='delete_MNAR_rank', cols_mis=10:11, 
                                                 n = 10)
print(gen_miss)
tail(gen_miss$miss_iterations, 25)
table(gen_miss$miss_iterations$variable)
table(gen_miss$miss_iterations$iter)

# Get particular iteration(s)
iter5 <- scenario_get_iteration(gen_miss, 5)
head(iter5$scenario_iteration, 25)
table(iter5$scenario_iteration$iter)

# Save
# gc()
scenario_save(gen_miss, save_path = here("simulated_data", "out", "one_iteration_test.parq"),
              optimized=FALSE)
df <- data.frame(read_parquet(here("simulated_data", "out", "one_iteration_test.parq")))
table(df$iter)
head(df, 20)

# Plot
i_plot <- iteration_plot(iter5$scenario_iteration)
print(i_plot)
s_plot <- iteration_plot(df, iter=3)
print(s_plot)

# Missing indicators
miss_ind <- create_missing_indicators(gen_miss)
tail(miss_ind[,-c(3:10)], 20)
tail(gen_miss$miss_iterations[order(gen_miss$miss_iterations$iter,
                                    gen_miss$miss_iterations$business_number), ], 20)
