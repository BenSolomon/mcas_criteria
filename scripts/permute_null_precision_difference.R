library(here)
source(here("utils/data_processing.R"))
library(tidyverse)
library(vegan)
library(broom)
library(here)
library(furrr)
library(future.apply)

# print("Reading data")
# df_gpt4 <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))
# 
# print("Calculate precision")
# set.seed(1234)
# df_precision <- df_gpt4 %>%  
#   # filter(i %in% sample(1:10000, 100)) %>% 
#   group_by(criteria) %>% 
#   nest() %>% 
#   mutate(data = map(data, function(df){
#     diagnosis_table <- table(df$i, df$diagnosis)
#     dist_mtx <- parallelDist::parDist(diagnosis_table, method = "bray", threads = 40)
#     broom::tidy(dist_mtx) %>% 
#       select(distance)
#   })) %>% 
#   unnest(data)
# 
# print("Writing data")
# write_csv(df_precision, here("data/diagnosis_precision.csv.gz"))

p <- 100
i <- 10000

print("### Reading data")
df <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))

print("### Calculating permutation")
perm_out <- difference_permutation_test(df, metric = "precision", permutations = p, gpt_iterations = i)

print("### Writing data")

saveRDS(perm_out, here(sprintf("data/precision_permutation_test_p%s_i%s.RDS", p, i)))