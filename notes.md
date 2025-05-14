
analyze_missing_data()



simulate_missing_data( df x, n/ number_of_sims, simulation parameters)

returns a) n datasets with missing data // or b) n md idicator matrices



a) has the advantage that its easy to proceed, but ram ...?
   (lazy load...?) but is i/o really the issue...?

b) needs transforms to use imputation algorithm

probably a list with data.frames


more a list with meta data .... 
also function to export the datasets ...  


impute(...)??
wenn wrapped as a function then it would be
lapply() und irgendwie status waere auch gut
da waere future.apply natuerlich gut

what about metadata...??


evaluate_simulation_experiments(..., )


n_algos
  list mit n imputed datasets
  
  oder list of lists
  
  
oder objektorientiert ...??



post_imputation_plots_distributions()
