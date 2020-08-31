#This function is for counting the dietary diversity 'raw score' (out of a total of 12 food groups)

sum_diet<-function(my_df){
  my_df %>% 
    select(starts_with("diet") & ends_with("yes")) %>% 
    rowSums(na.rm = T) %>% 
    as.data.frame() %>% 
    set_names("diet_sum")
           
}
