library(here)
source(here("utils/data_processing.R"))
# source(here("utils/figures.R"))
library(tidyverse)
library(vegan)
library(broom)
library(here)
library(furrr)
library(future.apply)

model <- "gpt4"

print("### Reading data")
read_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", model)
df <- read_csv(here(read_path))


print("### Calculating permutation")
perm_out <- difference_permutation_test(df, metric = "diversity", permutations = 1000, gpt_iterations = NULL)

print("### Writing data")
write_path <- sprintf("data/diversity_analysis/diversity_permutation_test_%s.RDS", model)
saveRDS(perm_out, here(write_path))