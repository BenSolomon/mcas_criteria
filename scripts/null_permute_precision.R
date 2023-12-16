library(tidyverse)
library(vegan)
library(broom)
library(here)
library(furrr)
library(future.apply)

source(here("util_functions.R"))
# 
# parse_diagnoses_mcas <- function(sim_results, consensus){
#   sim_results <- do.call(rbind, sim_results[consensus, ])
#   map(sim_results$diagnoses, process_diagnoses) %>% 
#     enframe(name = "iteration", value = "diagnosis") %>% 
#     unnest(diagnosis) %>% 
#     mutate(consensus = consensus)
# }
# 
# parse_mcas_rds <- function(sim_results){
#   bind_rows(
#     parse_diagnoses_mcas(sim_results, "consensus1"),
#     parse_diagnoses_mcas(sim_results, "consensus2")
#   )
# }
# 
# parse_diagnoses_alt <- function(sim_results){
#   tibble(criteria = rownames(sim_results)) %>% 
#     mutate(data = map(criteria, function(c){
#       do.call(rbind, sim_results[c,]) %>% 
#         mutate(iteration = 1:n()) %>% 
#         mutate(diagnosis = map(diagnoses, process_diagnoses)) %>% 
#         unnest(diagnosis) %>% 
#         select(iteration, diagnosis)
#     })) %>% 
#     unnest(data)
# }
# 
# 
# df_mcas <- tibble(file = list.files("data/chatgpt_output/name_n20000/", full.names = T)) %>% 
#   filter(grepl("RDS$", file)) %>% 
#   mutate(data = map(file, readRDS)) %>% 
#   mutate(data = map(data, parse_mcas_rds)) %>% 
#   unnest(data)
# 

df <- read_csv(here("data/compiled_all_chatgpt_diagnoses.csv")) %>% 
  unite("sample", file, iteration, sep = "__") 

# null_permute <- function(){
#   df <- df %>% 
#     group_by(consensus) %>% 
#     nest() %>% 
#     mutate(data = map(data, function(df){
#       df %>% 
#         head(100) %>% 
#         mutate(sample = sample(sample)) %>% 
#         mutate(count = 1) %>% 
#         pivot_wider(names_from = "diagnosis", values_from = "count", values_fill = 0, values_fn = sum) %>% 
#         # head(100) %>% 
#         column_to_rownames("sample") %>% 
#         vegan::vegdist(method = "jaccard") %>% 
#         broom::tidy()
#     })) %>% 
#     unnest(data) %>% 
#     group_by(consensus) %>% 
#     summarise(mean = mean(1-distance), median = median(1-distance))
#   list(
#     "mean" = deframe(df %>% select(-median)),
#     "median" = deframe(df %>% select(-mean))
#   )
# }
# 
# null_permute_data <- replicate(n = 10, null_permute())
# saveRDS(null_permute_data, here("data/null_permute_precision.RDS"))


####
# null_permute <- function(path, permutations){
#   path <- sprintf("%s/null_permute_precision_%s", 
#                   path, 
#                   format(Sys.time(), "%y%m%d_%H%M%S"))
#   if (file.exists(path)){file.remove(path)}
#   for (i in 1:permutations){
#     print(sprintf("%s >>> Starting permutation: %s/%s",
#                   format(Sys.time(), "%y%m%d_%H%M%S"), 
#                   i,permutations))
#     df %>% 
#       group_by(consensus) %>% 
#       nest() %>% 
#       mutate(data = map(data, function(df){
#         df %>% 
#           # head(100) %>%
#           mutate(sample = sample(sample)) %>% 
#           mutate(count = 1) %>% 
#           pivot_wider(names_from = "diagnosis", values_from = "count", values_fill = 0, values_fn = sum) %>% 
#           # head(100) %>% 
#           column_to_rownames("sample") %>% 
#           vegan::vegdist(method = "jaccard") %>% 
#           broom::tidy()
#       })) %>% 
#       unnest(data) %>% 
#       group_by(consensus) %>% 
#       summarise(mean = mean(1-distance), median = median(1-distance)) %>% 
#       mutate(iteration = i) %>% 
#       select(iteration, everything()) %>% 
#       write_csv(path, append=T, col_names=!file.exists(path))
#   }
# }
# 
# null_permute(here("data"), 5000)

# path <- sprintf("data/null_permute_precision_%s.RDS",
#                 format(Sys.time(), "%y%m%d_%H%M%S"))

# null_permute <- function(){
#   df <- df %>%
#     group_by(consensus) %>%
#     nest() %>%
#     mutate(data = map(data, function(df){
#       df %>%
#         head(100) %>%
#         mutate(sample = sample(sample)) %>%
#         mutate(count = 1) %>%
#         pivot_wider(names_from = "diagnosis", values_from = "count", values_fill = 0, values_fn = sum) %>%
#         # head(100) %>%
#         column_to_rownames("sample") %>%
#         vegan::vegdist(method = "jaccard") %>%
#         broom::tidy()
#     })) %>%
#     unnest(data) %>%
#     group_by(consensus) %>%
#     summarise(mean = mean(1-distance), median = median(1-distance))
#   list(
#     "mean" = deframe(df %>% select(-median)),
#     "median" = deframe(df %>% select(-mean))
#   )
# }
# 
# plan(multisession, workers = 40)
# x <- future_replicate(5000, null_permute(), future.seed = 1234)
# saveRDS(x, here(path))

# path <- here(sprintf("data/null_permute_precision_slurm_%s.csv",
path <- here(sprintf("data/null_permute_precision_bray_slurm_%s.csv",
                format(Sys.time(), "%y%m%d_%H%M%S")))

null_permute <- function(){
  iteration_string <- stringi::stri_rand_strings(1,25)
  df <- df %>%
    group_by(criteria) %>%
    nest() %>%
    mutate(data = map(data, function(df){
      df %>%
        # head(100) %>%
        mutate(sample = sample(sample)) %>%
        mutate(count = 1) %>%
        pivot_wider(names_from = "diagnosis", values_from = "count", values_fill = 0, values_fn = sum) %>%
        # head(100) %>%
        column_to_rownames("sample") %>%
        # vegan::vegdist(method = "jaccard") %>%
        vegan::vegdist(method = "bray") %>%
        broom::tidy()
    })) %>%
    unnest(data) %>%
    group_by(criteria) %>%
    summarise(mean = mean(1-distance), median = median(1-distance)) %>% 
    mutate(iteration = iteration_string) %>% 
    select(iteration, everything()) %>%
    write_csv(path, append=T, col_names=!file.exists(path))
  sprintf("%s >>> Iteration %s complete", format(Sys.time(), "%y%m%d_%H%M%S"), iteration_string)
}

plan(multisession, workers = 40)
future_replicate(5000, null_permute(), future.seed = 1234)
