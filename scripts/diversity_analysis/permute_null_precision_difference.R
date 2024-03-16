library(here)
source(here("utils/data_processing.R"))
library(tidyverse)
library(vegan)
library(broom)
library(here)
library(furrr)
library(future.apply)

model <- "gpt4"
p <- 1000
i <- 10000

print("### Reading data")
read_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", model)
df <- read_csv(here(read_path))

print("### Calculating permutation")
perm_out <- difference_permutation_test(df, metric = "precision", permutations = p, gpt_iterations = i)

print("### Writing data")
write_path <- sprintf("data/diversity_analysis/precision_permutation_test_%s_p%s_i%s.RDS", model, p, i)
saveRDS(perm_out, here(write_path))