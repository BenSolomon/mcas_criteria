library(tidyverse)
library(flextable)


tibble(criteria = list.files(here("data/disease_criteria"))) %>% 
  mutate(file = here(sprintf("data/disease_criteria/%s", criteria))) %>% 
  mutate(criteria = gsub("_criteria_symptoms\\.csv", "", criteria)) %>% 
  mutate(data = map(file, function(df){
    suppressMessages(read_csv(df)) %>% 
      mutate(feature = str_split(feature, ", "))
  })) %>% 
  select(-file) %>% 
  unnest(data) %>% 
  mutate(symptom = map_chr(feature, ~paste(., collapse = ", "))) %>% 
  select(-feature) %>% 
  flextable() %>% 
  colformat_char(na_str = "---") %>% 
  merge_v(j=1) %>% 
  theme_vanilla() %>% 
  fix_border_issues() %>% 
  print(preview = "docx")