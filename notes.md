
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


Set of macros 

 

Non-response (straightforward) 

Generate NR Flags (MANDATORY) 

Various “sampling” mechanisms 

SRS 

Bernoulli 

Pareto 

Cross-validation 

Controlled SRS 

Custom 

Extra programs: (OPTIONAL) 

Generate non-response probabilities 

(Logistic function…) 

Utility macros: 

Skinny <-> fat 

 

Imputation (pretty straightforward - OPTIONAL) 

Wrapper 

Inputs: NR pattern, simulation data 

Inputs: imputation code 

Purpose: wrapper runs the imputation code on each trial, outputs imputed data 

 

Analysis 

Prepare analysis files 

Calculating errors 

Calculating imputation rates 

Visualization 

GTL TEMPLATES 

PROC SGRENDER 

Three types of analysis: 

Predictive (mostly scatterplots) 

Distribution (kernel density plots, etc…) 

Estimator 

Analysis for a single simulation (one method, one non-response mechanism) 

Need ways to compare multiple methods 

Multiple non-response mechanism 

Multiple imputation methods 

Etc… 
