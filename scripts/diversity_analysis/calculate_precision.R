# Calculate precision

library(here)
source(here("utils/data_processing.R"))

models <- c("gpt3", "gpt4", "gpt4_icd")

for (m in models){
  print(sprintf("READING IN DATA FOR: %s", m))
  read_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", m)
  df <- read_csv(here(read_path))
  
  print(sprintf("CALCULATING PRECISION FOR: %s", m))
  df <- calculate_precision(df)
  
  print(sprintf("WRITING PRECISION DATA FOR: %s", m))
  out_path <- sprintf("data/diversity_analysis/diagnosis_precision_%s.csv.gz", m)
  write_csv(df, here(out_path))
}