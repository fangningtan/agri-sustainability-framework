#This R file contains all functions required for scoring transformed variables

linear_scoring <- function(var_name, single_var_config, my_df){
  #This function assigns scores to select_one ODK questions 
  #args: variable name, single_var config list (within indicator_config list), dataframe containing binary-transformed responses
  
  #1. Save the coefficients of the single_var config to a list 
  coeffs = single_var_config[["coeffs"]]
  
  #2. Map over the names of the coeffs list
  names(coeffs) %>%
    #3. Perform a matrix multiplication between the coeffs and the dataframe
    map(~my_df[,.x]*coeffs[[.x]]) %>% 
    reduce(`+`) %>% 
    #4. Convert output to a column
    as.data.frame() %>%
    #5. Rename resulting column with its name
    set_names(var_name) 
}


bin_scoring_row <- function(x, single_var_config){
  #This function assigns a score to x based on which bin of values it belongs to
  #It will be nested within the bin_indicator function
  #args: x (numeric value), single_var config list
  
  #1. Save the bin lower bound values to a list
  bins = single_var_config[["bin.lowers"]]
  
  #2. Save the corresponding scores of the bins to another list
  scores = single_var_config[["scores"]]
  
  #3. Using a for loop, starting from the largest bin lower bound, 
  #check whether x > bin lower bounds
  #if x> lower bound, it is assigned the corresponding score
  #otherwise the loop continues until the score is assigned
  for( i in rev(seq_along(bins))){
    if (x >= bins[[i]]){
      output<- scores[[i]]
      return(output)
    }
  }
  #This function assumes that for all variables in the framework, the variables do not take on negative values.
  #Hence if the function discovers a negative value, a warning will be produced.
  warning("Error: response value is less than 0")
}


bin_scoring <- function(var_name, single_var_config, my_df){
  #This function assigns a score to a column of values by mapping the bin_scoring_row function across a column
  #args: variable name, single_var config list (within indicator_config list), dataframe containing transformed responses 
  #tranformed bin variables will be numeric columns 
  #for select_multiple questions, the binary-transformed columns will be summed
  
  #1. Save the dataframe column name  to a vector
  colnm <- single_var_config[["colnm"]]
  
  #2. Save the bin lower bound values to a list
  bins = single_var_config[["bin_lowers"]]
  
  #3. Save the corresponding scores of the bins to another list
  scores = single_var_config[["scores"]]
  
  #4. Map the function over the dataframe my_df
  output <- map_dbl(my_df[,colnm], ~bin_scoring_row(.x, single_var_config)) %>%
    as.data.frame() %>% 
    set_names(var_name)
  
  return(output)
}

mean_scoring<- function(aggregate_name, single_aggregate_config, my_df){
  #This function assign scores to aggregates (indicators, themes, dimensions) by taking the mean of the aggregates' components.
  #args: aggregate name, single_aggregate config list (within indicator_config list), dataframe with columns of variables which make up the aggregate
  #It makes use of the linear_scoring function to calculate the mean.
  
  #1. Save the inputs/components of the aggregate to a list
  input_names <- single_aggregate_config[["inputs"]] 
  #2. Create a dummy_config containing length(input_names) coefficients 
  # The coefficients are all 1/length(input_names) as it assumes that all inputs have the same weight
  dummy_config <- map(input_names, ~1/length(input_names))
  names(dummy_config)<-input_names
  dummy_config <- list(coeffs=dummy_config)
  
  #3. Use the linear_scoring function to calculate the mean via matrix multiplication 
  output<-linear_scoring(aggregate_name, dummy_config, my_df)
  return(output)
  
}


scoring<-function(name, single_config, my_df){
  #This function nests the 3 different scoring functions into one function. 
  #It switches executes functions based on the function type of the single (variable/aggregate) config list.
  #args: variable/aggregate name, single variable/aggregate list, dataframe containing transformed responses
  
  #1. Save the function type of the single (variable/aggregate) config to a vector 
  function_type=single_config[["type"]]
  
  #2. Apply the corresponding function based on the function type.
  switch(function_type,
         linear = linear_scoring(name, single_config, my_df),
         bin = bin_scoring(name, single_config, my_df),
         mean = mean_scoring(name, single_config, my_df))
}