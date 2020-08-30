#This R file contains the functions for obtaining the metadata of ODK survey questions.

#1. The getChoicesLabelsList function creates a list containing machine-readable names (name) 
#and human-readable names (label::English)

getChoicesLabelsList<-function(labelsChoices_df, choice_prefix){
  option_rows <- filter(labelsChoices_df, list_name==choice_prefix)
  
  machineNames <-option_rows %>% 
    select("name") %>% 
    pull() %>% 
    list()
  
  humanNames <- option_rows %>% 
    select("label::English") %>% 
    pull() %>% 
    list()
  
  choicesLabels <- list(machineNames=machineNames, humanNames=humanNames)
  return(choicesLabels)
  
}
#This function will be mapped over the list of (unique) choice prefixes (list_name in the XLSForm) 
#This creates a nested list containing a list of machine- and human-readable names for each choice prefix.


#2. The getQnLabelList function creates a list containing the question title, type and choice prefix.

getQnLabelList<-function(labelsQns_df, question_name){
  
  qn_title<-labelsQns_df[question_name, ] %>% 
    select("label::English") %>% 
    pull()
  
  qn_type<-labelsQns_df[question_name, ] %>% 
    select(type) %>% 
    separate(type, into = c("qn_type", NA), sep=" ") %>% 
    pull() 
  
  qn_choicePrefix<-labelsQns_df[question_name, ] %>% 
    select(type) %>% 
    separate(type, into = c(NA, "prefix"), sep=" ") %>% 
    pull()
  
  QnLabel <- list(qn_title=qn_title, qn_type=qn_type, qn_choicePrefix=qn_choicePrefix)
  return(QnLabel)
  
}
#This function will be mapped over the list of questions names to create a nested list of question metadata.
#Each list in the nested list contains the question metadata for each question.


#The two functions have to be used together to obtain all the question meta-data needed.
#The qn_choicePrefix key will link the list created by getQnLabelList to the list created by getChoicesLabelsList