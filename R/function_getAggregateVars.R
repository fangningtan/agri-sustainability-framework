#This function obtains the names of columns/variables which belong to a certain aggregate (i.e. dimension/theme/indicator).
#This will be used in report generation to find out which plots should belong in a particular (segment of a) report.

getAggregateVars <- function(aggregate_type, aggregate_name, my_indicator_config){
  #arg1: aggregate type can refer to dimension/theme/indicator, 
  #arg2: aggregate name refers to the name of the aggregate
  #these two arguments need to be placed between ""
  #arg3: indicator config containing the framework scoring info
  
  vars_list <- names(my_indicator_config)[lapply(my_indicator_config, function(x) x[[aggregate_type]]) == aggregate_name]
  
  vars_list <- gsub("_score", "", vars_list)
  return(vars_list)
}