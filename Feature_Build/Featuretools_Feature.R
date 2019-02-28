
library(featuretoolsR)
library(magrittr)
  
 apply_ft_trans <- function (.data) {
              dplyr::select(.data, contains('id'), -contains('qid'), contains('question')) %>% 
              featuretoolsR::as_entityset(index = "id", entity_id = "base", id = "quora") %>% 
              featuretoolsR::dfs(target_entity = names(es$entity_dict)[1], max_depth = 2) %>% 
              tidy_feature_matrix(remove_nzv = T, nan_is_na = T) %>% 
              write_csv('feature_tools_trans.csv')
 }
 


 
 
 
 