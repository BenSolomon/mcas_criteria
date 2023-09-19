library(tidyverse)
library(vegan)
library(here)

df <- read_csv(here("data/compiled_mcas_chatgpt_diagnoses.csv"))

permutation_pair_iteration <- function(randomize = T){
  if (randomize == T){
    df <- df %>% mutate(diagnosis = sample(diagnosis, n()))
  }
  div <- vegan::diversity(table(df$consensus, df$diagnosis))
  unname(div[1] - div[2])
}

# Generate null sampling distribution
set.seed(1234)
div_rep <- replicate(10000, permutation_pair_iteration())

saveRDS(div_rep, here("data/mcas_criteria_null_distribtion.RDS"))