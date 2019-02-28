############################  Quora Question Pairs ########################

Quora Question Pairs Competition

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(featuretoolsR)
library(lime)
library(shapper)
library(text2vec)

# Read in Data ------------------------------------------------------------

train <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/train.csv")
test <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/test.csv")
sample_submission <- read_csv("/Users/samtaylor/Downloads/quora-question-pairs/sample_submission.csv")

# Glove  ------------------------------------------------------------------

tokens = space_tokenizer(train$question1)
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5L)
vectorizer = vocab_vectorizer(vocab)
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)

wv_context = glove$components
dim(wv_context)


word_vectors = wv_main + t(wv_context)





library("DALEX2")
Y_train <- HR$status
x_train <- HR[ , -6]



# Let's build models
library("randomForest")
set.seed(123)
model_rf <- randomForest(x = x_train, y = Y_train)


library(shapper)
p_function <- function(model, data) predict(model, newdata = data, type = "prob")

ive_rf <- individual_variable_effect(model_rf, data = x_train, predict_function = p_function,
                                     new_observation = x_train[1:2,], nsamples = 50)

View(ive_rf)


plot(ive_rf)

colnames(ive_rf)

ive_rf %>% filter(gender =='male') %>% 
ggplot(aes(x=fct_reorder(as.factor(str_to_title(`_vname_`)),`_attribution_`), y=`_attribution_`, fill = `_attribution_` >0 )) + 
  geom_col(show.legend = FALSE) + coord_flip() + 
  facet_wrap(~str_to_title(`_ylevel_`)) + theme_bw() + 
 scale_fill_manual(values=c("red", "darkgreen")) + xlab("Element of Claim") + ylab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
