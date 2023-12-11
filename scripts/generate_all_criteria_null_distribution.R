library(tidyverse)
library(here)
library(vegan)

df <- read_csv(here("data/compiled_all_chatgpt_diagnoses.csv"))

n <- 10000

pairwise_div_difference <- function(df){
  df_div <- enframe(vegan::diversity(table(df$criteria, df$diagnosis)))
  df_diff <- data.frame(t(combn(unique(sort(df$criteria)),2))) %>% 
    left_join(df_div, by = c("X1"="name")) %>% 
    left_join(df_div, by = c("X2"="name")) %>% 
    unite("pair", X1, X2, sep = ".") %>%
    mutate(entropy_difference = value.x - value.y) %>% 
    select(-contains("value"))
  deframe(df_diff)
}

pairwise_div_iteration <- function(){
  df %>% 
    count(criteria) %>% 
    mutate(diagnosis = map(n, function(x) sample(df$diagnosis, x, replace = T))) %>% 
    select(-n) %>% 
    unnest(diagnosis) %>% 
    pairwise_div_difference(.)
}

set.seed(1234)
pairs_permutation_distributions <- data.frame(t(replicate(n, pairwise_div_iteration())))
pairs_permutation_distributions %>% 
  pivot_longer(everything(), names_to = "pair", values_to = "entropy_difference") %>% 
  write_csv(here("data/all_criteria_null_difference_distributions.csv"))

permutation_pair_iteration <- function(pair, randomize = T){
  df <- df %>% 
    filter(criteria %in% pair) 
  if (randomize == T){
    df <- df %>% mutate(diagnosis = sample(diagnosis, n()))
  }
  div <- vegan::diversity(table(df$criteria, df$diagnosis))
  unname(div[1] - div[2])
}

pairs_list <- as.list(as.data.frame(combn(unique(df$criteria),2)))
names(pairs_list) <- sapply(pairs_list, paste, collapse = ".")

set.seed(1234)
pairs_permutation_distributions <- lapply(pairs_list, function(x) replicate(n, permutation_pair_iteration(x)))
names(pairs_permutation_distributions) <- lapply(pairs_list, function(x) paste(x, collapse = "-"))

data.frame(pairs_permutation_distributions) %>%
  pivot_longer(cols = everything(), names_to = "pair", values_to = "entropy_difference") %>% 
  write_csv(here("data/all_criteria_pairwise_null_difference_distributions.csv"))