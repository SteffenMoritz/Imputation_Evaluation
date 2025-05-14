source("~/Imputation_Evaluation/R/simulate_missing_data.R") 
source("~/Imputation_Evaluation/R/impute_simulations.R") 
source("~/Imputation_Evaluation/R/evaluate_simulation_runs.R") 


### Function Definitions ####

# Function to impute with a package e.g. missForest
missForest_imputer <- function(df) {
  out <- missForest::missForest(df, ntree = 100, maxiter = 5, verbose = FALSE)
  out$ximp
}

0
1
n


### Procudure ####
#class simulations
md_sims  <- simulate_missing_data(nhanes, cols_mis= 1, n = 100)

save_simulations(md_sims, file =)
get_simulation(number)

print(md_sims)

simulate_missing_data()
class simulations

save(obj)
simulations_save(md_sims, file =)
simulations_get(class simulation)
simulations_print
simulations_plot


impute_simulations
impute_simulations_save
impute_simulations_plot
impute_simulations_print

imputed_run1 <- impute_simulations(md_sims,
                              impute_fun = missForest_imputer, 
                              alg_name = "missForest")


# hier bereits metrics kalkulieren ... ?  
imputed_run2 <- impute_simulations(md_sims, keep_results = F, 
                                   impute_fun = knn_imputer, 
                                   alg_name = "knn")


# hier muss man sich sicher sein, dass alle auf gleichen md_sims erstellt sind...
compare_imputed_simulations(imps_obj = c(imputed_run1, imputed_run2, ...)
compare_imputed_simulations_save
compare_imputed_simulations_plot
compare_imputed_simulations_print

# hier muss man sich sicher sein, dass alle auf gleichen md_sims erstellt sind...
evaluate_simulation_runs( imps_obj = c(imputed_run1, imputed_run2, ...) 

res <- evaluate_simulation_runs( imps_obj = imputed_runs, original = nhanes) 


simulate_missing_data(nhanes, cols_mis= 1, n = 100) %>% impute_simulations(funs) %>% evaluate_simulation_runs()