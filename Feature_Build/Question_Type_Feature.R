
#############################  QUESTION TYPE FEATURE ############################

# identifies what type of question is being asked

# Load Packages -----------------------------------------------------------

library(tidyverse)

# Question Type Feature ---------------------------------------------------

question_type  <- function (.data) {
  dplyr::select(.data, contains('id'), -contains('qid'), contains('question')) %>% 
  dplyr::transmute(question1 = str_to_lower(question1), question2 =  str_to_lower(question2)) %>% 
  dplyr::mutate(who_q1  = str_detect(question1, 'who'),
         what_q1 = str_detect(question1,'what'),
         when_q1 = str_detect(question1,'when'),
         where_q1 = str_detect(question1,'where'),
         why_q1 = str_detect(str_to_lower(question1),'why'),
         how_q1 = str_detect(str_to_lower(question1),'how'),
         difference_between_q1 = str_detect(str_to_lower(question1),'difference between'),
         who_q2  = str_detect(str_to_lower(question2),'who'),
         what_q2 = str_detect(str_to_lower(question2),'what'),
         when_q2 = str_detect(str_to_lower(question2),'when'),
         where_q2 = str_detect(str_to_lower(question2),'where'),
         why_q2 = str_detect(str_to_lower(question2),'why'),
         how_q2 = str_detect(str_to_lower(question2),'how'),
         difference_between_q2 = str_detect(str_to_lower(question2),'difference between')) %>% 
  dplyr::select(-contains('question')) %>% 
  readr::write_csv('question_type_features.csv')
}
