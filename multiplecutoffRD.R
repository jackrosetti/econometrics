library(rdd)

# Define the function
rdd_multiple_cutoffs <- function(data, outcome_var, rdd_var, cutoffs) {
  # data: the data frame containing the variables
  # outcome_var: the name of the outcome variable
  # rdd_var: the name of the RDD variable
  # cutoffs: a vector of cutoff values to use
  
  # Create an empty data frame to store the results
  results <- data.frame(Cutoff = numeric(),
                        Estimate = numeric(),
                        Std_Error = numeric(),
                        t_value = numeric(),
                        p_value = numeric(),
                        stringsAsFactors = FALSE)
  
  # Loop through the cutoff values and run RDD regressions for each
  for (cutoff in cutoffs) {
    # Create a new variable indicating which side of the cutoff each observation falls on
    data$Above_Cutoff <- ifelse(data[[rdd_var]] > cutoff, 1, 0)
    
    # Run the RDD regression
    model <- rdd_reg_lm(data[[outcome_var]] ~ data[[rdd_var]] + data$Above_Cutoff, cutpoint = cutoff)
    
    # Extract the relevant information from the model object and store it in the results data frame
    results[nrow(results) + 1,] <- c(cutoff,
                                     model$coef[2],
                                     model$se[2],
                                     model$tstat[2],
                                     model$p[2])
  }
  
  # Return the results data frame
  return(results)
}
