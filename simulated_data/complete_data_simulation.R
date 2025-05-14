
library(here)
library(arrow)
library(stringr)

# Define the function
simulate_data <- function(seed, size, p_prov, naics_val, p_employees, mu, sigma, 
                          p_inconsist_rev, save=FALSE, folder_to_save=NULL
                          ) 
  {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Create a record unit_id
  # business_number <- paste0("S", str_pad(1:size, nchar(as.character(size)), pad = "0"))
  business_number <- 1:size
  
  # Generate province and naics based on input labels and probabilities 
  province <- sample(names(p_prov), size, replace=TRUE, prob=unlist(p_prov))
  naics <- sample(naics_val, size, replace=TRUE)
  
  # Number of employees to generate for each size of business based on input labels and probabilities 
  employees_size <- round(unlist(p_employees)*size)
  # Make the different sizes sum to the total size
  if (sum(employees_size)!=size){
    employees_size[1] <- size - sum(employees_size[-1])
  }
  
  # Generate number of employees
  employees <- c(sample(1:4, employees_size[1], replace=TRUE),
                 sample(5:9, employees_size[2], replace=TRUE),
                 sample(10:19, employees_size[3], replace=TRUE),
                 sample(20:49, employees_size[4], replace=TRUE),
                 sample(50:99, employees_size[5], replace=TRUE),
                 sample(100:999, employees_size[6], replace=TRUE))
  
  # Attach a category to nb of employees
  employees_cat <- cut(employees,
                       breaks = c(0, 4, 9, 19, 49, 99, Inf),
                       labels = c('1_1-4', '2_5-9', '3_10-19', '4_20-49', '5_50-99', '6_100+'))
  
  # Small business : less than 100 workers
  small_business <- ifelse(employees < 100, 1, 0)
  small_business <- factor(small_business,
                                  levels = c(0, 1),
                                  labels = c("Not Small Business", "Small Business"))
  
  # Generate a quarterly revenue from a log-normal distribution
  # Rounding to a negative number of digits means rounding to a power of ten
  revenue_q1 <- round(rlnorm(n = size, meanlog = mu+log(employees), sdlog = sigma), -1)
  revenue_q2 <- round(rlnorm(n = size, meanlog = mu+log(employees), sdlog = sigma), -1)
  revenue_q3 <- round(rlnorm(n = size, meanlog = mu+log(employees), sdlog = sigma), -1)
  revenue_q4 <- round(rlnorm(n = size, meanlog = mu+log(employees)+3, sdlog = sigma), -1)         
  
  # Revenue is the sum of its quarterly components, to which we add a noise for some as consistencies
  revenue <- abs(revenue_q1 + revenue_q2 + revenue_q3 + revenue_q4 + 
    sample(c(-1000, 0, 1000), size, replace=TRUE, prob=c(p_inconsist_rev/2, 1-p_inconsist_rev, p_inconsist_rev/2)))
  
  # Revenue - Expenses = EBT
  # Revenue >= 0; Expenses >= 0
  # EBT (earnings before Tax) >= 0; EAT (Earnings after tax) >= 0
  # EAT = EBT - tax
  # tax <= .55*EBT
  # Dividend <= EAT
  
  # Expenses
  # Expenses = .5*revenue + 2*sqrt(revenue) + i.i.d.
  expenses <- round( .5*revenue + 2*sqrt(revenue) + rnorm(n = size, mean = mu, sd = sigma), -1)         
  
  # EBT (earnings before Tax)
  ebt <- revenue - expenses + 
    sample(c(-1000, 0, 1000), size, replace=TRUE, prob=c(p_inconsist_rev/2, 1-p_inconsist_rev, p_inconsist_rev/2))
  
  # tax rate, between 11% and 25% for small businesses, 30% to 55% otherwise
  # tax_rate <- round(runif(size, min=ifelse(small_business==1, 11, 30), max=ifelse(small_business==1, 25, 55)), 2)
  tax_rate <- round(.55 - .3*as.numeric(small_business) - .01*log(employees) + 
    sample(c(0, .15), size, replace=TRUE, prob=c(1-p_inconsist_rev, p_inconsist_rev)), 2)

  # tax 
  tax <- round(ebt*tax_rate, 0) 
  
  # EAT (Earnings after tax)
  eat <- ebt - tax + sample(c(-1000, 0, 1000), size, replace=TRUE, prob=c(p_inconsist_rev/2, 1-p_inconsist_rev, p_inconsist_rev/2))
  
  # Dividend 
  # Dividend = 110 + .35*eat + .2*revenue
  dividend <- round(110+.35*eat + .1*revenue + rnorm(n = size, mean = mu, sd = sigma), -1)
  
  # Can be added
  # Dividend + Debt_payment + Reinvestment <= Profit
  # Dividend >= 0; Debt_payment >= 0; Reinvestment >= 0;
  
  # Put together the dataframe
  simulated <- data.frame(business_number, province, naics, employees, employees_cat, small_business, 
                          revenue_q1, revenue_q2, revenue_q3, revenue_q4, revenue, 
                          expenses, ebt, tax, eat, dividend)
  
  # Shuffle the dataframe
  simulated <- simulated[sample(size), ]
  
  
  # Save if requested 
  if (save==TRUE){
    write_parquet(simulated, here(folder_to_save, 'simulated_complete_data.parq'))
  }
  
  # Return the simulated df, and the simulated with missing
  return(simulated)
  
  }
  

seed <- 11
size <- 10000
p_inconsist_rev <- .1
# Parameters for log-normal distribution
mu <-  5 ; sigma <- 1

p_prov <- list(ON=.39, QC=.23, BC=.14, AB=.12, MB=.04, SK=.03, NS=.02, NB=.02, NL=.01)
naics_val <- c("11", "21", "22", "23", "31-33", "41", "44-45", "48-49", "51",
               "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "91")
p_weight_prov <- c(.39, .23, .14, .12, .04, .03, .02, .02, .01)
p_employees <- c('1_4'=.57, '5_9'=.18, '10-19'=.12, '20-49'=.08, '50-99'=.03, '>99'=.02)

save <- TRUE
folder_to_save <- 'simulated_data'


simulation <- simulate_data(seed=seed, size=size, p_prov=p_prov, 
                           naics_val=naics_val, p_employees=p_employees, mu=mu, sigma=sigma,
                           p_inconsist_rev=p_inconsist_rev, save=save,
                           folder_to_save=folder_to_save)

