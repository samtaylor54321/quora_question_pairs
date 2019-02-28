
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)

# Load Data ---------------------------------------------------------------

train <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/train.csv")
test <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/test.csv")

class(train$question1)

# EDA ---------------------------------------------------------------------

train %>% select(qid1, is_duplicate) %>% count (qid1,is_duplicate) %>% 
  ggplot(aes(x=log(n))) + geom_histogram(aes(fill = as.factor(is_duplicate)),binwidth = .5, alpha = 1/3) 

# Graph Database ----------------------------------------------------------

# split ngrams/bigrams/trigrams

q1_unigram <- train %>% 
  select(question1) %>% 
  unnest_tokens(unigram, question1, token ='ngrams', n=1) 

q1_bigram <- train %>% 
  select(question1) %>% 
  unnest_tokens(bigram, question1, token ='ngrams', n=2) 

q1_trigram <- train %>% 
  select(question1) %>% 
  unnest_tokens(trigram, question1, token ='ngrams', n=3) 

# split ngrams/bigrams/trigrams

q1_unigram %>% 
  group_by(unigram) %>% 
  summarise(count = n(),
            distinct_questions = n_distinct(qid1)) %>% 
  arrange(desc(count))

q1_bigram %>% 
  group_by(bigram) %>% 
  summarise(count = n(),
            distinct_questions = n_distinct(qid1)) %>% 
  arrange(desc(count))
  
q1_trigram %>% 
  group_by(trigram) %>% 
  summarise(count = n(),
            distinct_questions = n_distinct(qid1)) %>% 
  arrange(desc(count))

# Separate Ngrams and count

bigrams_separated <- q1_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>% 
  mutate(n = log(n)) %>% top_n(600)

# Create Graph

bigram_graph <- bigrams_separated %>%
  graph_from_data_frame()

# Posh graph 

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)  + 
  theme_void()

# Correlation Graph -------------------------------------------------------

question1 <- train %>% select(id, qid1, question1) %>% 
  unnest_tokens(word, question1)  %>%
  filter(!word %in% stop_words$word) %>% distinct() %>% 
  group_by(word) %>% 
  filter(n() > 20) %>% 
  pairwise_cor(word, qid1, sort = TRUE)

question1 %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



