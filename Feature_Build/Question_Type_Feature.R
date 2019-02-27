
#############################  QUESTION TYPE FEATURE ############################

# identifies what type of question is being asked

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)

# Load Data ---------------------------------------------------------------

train <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/train.csv")
test <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/test.csv")

# Question Type Feature ---------------------------------------------------

question_type_features <- train %>% mutate(id = id, qid1 = qid1,qid2 = qid2, question1 = str_to_lower(question1), question2 =  str_to_lower(question2)) %>% 
  mutate(who_q1  = str_detect(question1, 'who'),
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
         difference_between_q2 = str_detect(str_to_lower(question2),'difference between'),
         who_question = who_q1 == who_q2,
         what_question = what_q1 == what_q2,
         when_question = when_q1 == when_q2,
         where_question = where_q1 == where_q2,
         why_question = why_q1 == why_q2,
         how_question = how_q1 == how_q2,
         difference_between_question = difference_between_q1 == difference_between_q2) %>% 
  select(id, question1, question2, who_question, what_question, when_question, where_question, why_question, how_question, difference_between_question)


View(head(question_type_features))
