library(tidyverse)
library(vegan)
library(broom)
library(here)
library(furrr)
library(future.apply)

source(here("util_functions.R"))

df <- read_csv(here("data/compiled_all_chatgpt_diagnoses.csv")) 

df_precision <- df %>%  
  unite("sample", file, iteration, sep = "__")  %>%
  group_by(criteria) %>% 
  nest() %>% 
  mutate(data = map(data, function(df){
    df %>% 
      mutate(count = 1) %>% 
      pivot_wider(names_from = "diagnosis", values_from = "count", values_fill = 0, values_fn = sum) %>% 
      column_to_rownames("sample") %>% 
      vegan::vegdist(method = "bray") %>% 
      broom::tidy()
  })) %>% 
  unnest(data)

path <- here(sprintf("data/null_permute_precision_comparison_bray_slurm_%s.csv",
                format(Sys.time(), "%y%m%d_%H%M%S")))

# null_permute <- function(){
#   iteration_string <- stringi::stri_rand_strings(1,25)
#   df_precision_summary <- df_precision %>% 
#     ungroup() %>% 
#     mutate(distance = sample(distance)) %>% 
#     group_by(criteria) %>% 
#     summarise(mean = (1-mean(distance)), median = (1-median(distance))) 
#   as.data.frame(t(combn(df_precision_summary$criteria, 2))) %>% 
#     left_join(df_precision_summary, by = c("V1" = "criteria")) %>% 
#     left_join(df_precisisampoon_summary, by = c("V2" = "criteria")) %>% 
#     mutate(mean_diff = mean.x - mean.y,
#            median_diff = median.x - median.y) %>% 
#     mutate(iteration = iteration_string) %>% 
#     select(iteration, V1, V2, contains("diff")) %>% 
#     write_csv(path, append=T, col_names=!file.exists(path))
#   cat(sprintf("%s >>> Iteration %s complete", format(Sys.time(), "%y%m%d_%H%M%S"), iteration_string))
# }


null_permute <- function(){
  iteration_string <- stringi::stri_rand_strings(1,25)
  as.data.frame(t(combn(unique(df_precision$criteria), 2))) %>% 
    mutate(data = map2(V1,V2, function(v1,v2){
      df_permute <- df_precision %>% 
        filter(criteria == v1 | criteria == v2) %>% 
        ungroup() %>% 
        mutate(distance = sample(distance)) %>% 
        group_by(criteria) %>% 
        summarise(mean = (1-mean(distance)), median = (1-median(distance)))
      as.data.frame(t(combn(df_permute$criteria, 2))) %>% 
        left_join(df_permute, by = c("V1" = "criteria")) %>% 
        left_join(df_permute, by = c("V2" = "criteria")) %>% 
        mutate(mean_diff = mean.x - mean.y,
               median_diff = median.x - median.y) %>% 
        select(V1, V2, contains("diff")) %>% 
        unite(comp, V1, V2)
    })) %>% 
    unnest_wider(data) %>% 
    mutate(iteration = iteration_string) %>% 
    select(iteration, V1, V2, contains("diff")) %>% 
    write_csv(path, append=T, col_names=!file.exists(path))
  cat(sprintf("%s >>> Iteration %s complete", format(Sys.time(), "%y%m%d_%H%M%S"), iteration_string))
}

# replicate(2, null_permute())

plan(multisession, workers = 40)
future_replicate(5000, null_permute(), future.seed = 1234)
    


