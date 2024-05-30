library(tidyverse)
library(here)

set.seed(1234)
iterations <- 20000

query_diagnoses <- function(df, n_diagnoses=5, n_symptoms = 5){
  system_sample <- sample(1:nrow(df), n_symptoms, replace = T)
  feature_list <- unique(unlist(map(df$feature[system_sample], sample, 1)))
  feature_string <- paste(feature_list, collapse = ", ")  
  return(feature_string)
}

df_criteria <- tibble(criteria = list.files(here("data/disease_criteria"))) %>% 
  mutate(file = here(sprintf("data/disease_criteria/%s", criteria))) %>% 
  mutate(criteria = gsub("_criteria_symptoms\\.csv", "", criteria)) %>% 
  mutate(data = map(file, function(df){
    suppressMessages(read_csv(df)) %>% 
      mutate(feature = str_split(feature, ", "))
  })) 

df_criteria %>% 
  mutate(symptoms = map(data, ~replicate(iterations, query_diagnoses(., n_diagnoses = 10)))) %>% 
  unnest(symptoms) %>% 
  mutate(i = 1:n(), .by = criteria) %>% 
  select(criteria, i, symptoms) %>% 
  write_csv(here("data/criteria_query_iterations.csv"))
