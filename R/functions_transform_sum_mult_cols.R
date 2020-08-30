#This R file contains the sum_mult_cols function for transforming the binary-transformed variables.
#The preceding transformation steps (mainly one-hot encoding) can be executed using existing functions from other packages.

sum_mult_cols<-function(my_df, var_prefix){
  #This function creates a column containing the sum of columns with the same prefix
  #It should be applied to binary-transformed variables of select_multiple questions.
  #args: dataframe of binary-transformed responses, prefix shared by a set of columns from the same question
  
  #The prefixes can be obtained from the XLSForm (see notebook for script to create list containing all prefixes)
  #The function can be mapped over a list containing all the variable prefixes.
  
  my_df %>% 
    select(contains(var_prefix)) %>% 
    #Exclude the columns [var_prefix]0 and [var_prefix]99 because they represent 'None' and 'Other' responses respectively
    select(-matches(paste0(var_prefix,0))) %>%
    select(-matches(paste0(var_prefix,99))) %>% 
    #Sum the rest of columns
    rowSums(na.rm = T) %>% 
    #Save the output sum as a dataframe column
    as.data.frame() %>% 
    #Rename the output column [var_prefix]Sum
    set_names(paste0(var_prefix,"Sum")) 
  
}
