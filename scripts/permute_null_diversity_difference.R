library(here)
source(here("utils/data_processing.R"))
# source(here("utils/figures.R"))
library(tidyverse)
library(vegan)
library(broom)
library(here)
library(furrr)
library(future.apply)


# n <- 5000 # Number of permutations
# 
# print("### Reading data")
# df <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))
# 
# # Randomize diagnoses
# null_pairwise_div_permute <- function(df){
#   out <- as.data.frame(t(combn(unique(sort(df$criteria)),2))) %>%
#     mutate(data = map2(V1, V2, function(v1,v2){
#       df %>%
#         ungroup() %>%
#         filter(criteria == v1 | criteria == v2) %>%
#         mutate(diagnosis = sample(diagnosis, replace = F))})) %>%
#     select(-contains("V")) %>%
#     unnest(data) %>%
#     pairwise_diversity_difference()
#   return(out)
# }
# 
# print("### Allocating workers")
# plan(multisession, workers = 40)
# 
# print("### Executing parallel operations")
# df_permute_null_div_difference <- future_replicate(n, null_pairwise_div_permute(df), future.seed = 1234)
# 
# print("### Writing data")
# df_permute_null_div_difference <- as.data.frame(t(df_permute_null_div_difference)) %>%
#   pivot_longer(everything(), names_to = "pair", values_to = "diversity_difference") %>%
#   write_csv(here("data/null_diversity_difference.csv.gz"))

# n <- 5000 # Number of permutations

print("### Reading data")
df <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))

print("### Calculating permutation")
perm_out <- difference_permutation_test(df, metric = "diversity", permutations = 1000, gpt_iterations = NULL)

print("### Writing data")

saveRDS(perm_out, here("data/diversity_permutation_test.RDS"))