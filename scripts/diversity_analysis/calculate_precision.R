# Calculate precision
library(here)
source(here("utils/data_processing.R"))

models <- list.files(here("data/processed_diagnoses"), pattern = "gz$") %>% 
  str_split("diagnoses_|_icd|.csv") %>% 
  sapply(., function(x) x[2]) %>% 
  unique()

use_icd <- TRUE
n_iterations <- NULL # Number of response iterations to include

if (use_icd){models <- str_glue("{models}_icd")}

combined_list <- list()

for (m in models){
  print(sprintf("READING IN DATA FOR: %s", m))
  read_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", m)
  df <- read_csv(here(read_path))
  
  print(sprintf("CALCULATING PAIRWISE DISTANCE FOR: %s", m))
  df <- calculate_precision(df,n_iterations)
  
  print(sprintf("SUMMARIZING METRICS FOR: %s", m))
  df <- df %>% summarise(
    model = m,
    n = n(),
    mean = mean(distance),
    max = max(distance),
    min = min(distance),
    sd = sd(distance),
    se = sd(distance) / sqrt(n()),
    .by = "criteria"
  ) 
  
  combined_list <- c(combined_list, list(df))
}

print("WRITING DATA")
if (use_icd){
  out_path <- here("data/diversity_analysis/compiled_icd_diagnosis_precision.csv")
} else {
  out_path <- here("data/diversity_analysis/compiled_diagnosis_precision.csv")
}
df <- bind_rows(combined_list)
write_csv(df, here(out_path))
