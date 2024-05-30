# Parse JSON files from ChatGPT to generate quick-to-load
# csvs of ChatGPT response

library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))

get_parent_dir <- function(model){
  output <- case_when(
    grepl("gpt", model) ~ "chatgpt_json_output",
    grepl("claude", model) ~ "claude_json_output",
    grepl("gemini", model) ~ "gemini_json_output",
    TRUE ~ NA
  )
  if (is.na(output)){stop("Invalid model")}
  return(output)
}

processing_pipeline <- function(model){
  
  print(sprintf("STARTING MODEL: %s", model))
  
  parent_dir <- get_parent_dir(model)
  json_path <- here(str_glue("data/{parent_dir}/{model}/"))
  compiled_path <- here(str_glue("data/processed_diagnoses/diagnoses_{model}.csv.gz"))
  unique_path <- here(str_glue("data/unique_diagnoses/unique_diagnoses_{model}.csv.gz"))
  
  print(json_path)
  print(compiled_path)
  print(unique_path)
  
  # Read in data
  df <- process_json(json_path)
  # Write compiled data
  write_csv(df, compiled_path)
  # Write unique diagnoses
  df %>% 
    count(diagnosis) %>% 
    write_csv(unique_path)
}

processing_pipeline("gpt-3.5-turbo-1106")
processing_pipeline("gpt-4-turbo-preview")
processing_pipeline("claude-3-haiku-20240307_t1-0")
processing_pipeline("claude-3-opus-20240229_t1-0")
processing_pipeline("gemini-1.0-pro-002_t1-0")
processing_pipeline("gemini-1.5-flash-preview-0514_t1-0")
processing_pipeline("gemini-1.5-pro-001_t1-0")


# df_gpt3.5 <- process_json(here("data/chatgpt_json_output/gpt-3.5-turbo-1106/"))
# df_gpt4.0 <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))
# df_claude3_haiku_t1.0 <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t1-0/"))
# df_claude3_haiku_t0.1 <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t0-1/"))
# df_claude3_opus_t1.0 <- process_json(here("data/claude_json_output/claude-3-opus-20240229_t1-0/"))
# df_gemini1.0_pro_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.0-pro-002_t1-0/"))
# df_gemini1.0_pro_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.0-pro_t1-0/"))
# # df_gemini1.5_flash_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.5-flash-latest_t1-0/"))
# df_gemini1.5_flash_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.5-flash-preview-0514_t1-0/"))
# df_gemini1.5_pro_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.5-pro-001_t1-0/"))
# 
# 
# # Write analysis data files
# write_csv(df_gpt3.5, here("data/processed_diagnoses/diagnoses_gpt3.5.csv.gz"))
# write_csv(df_gpt4.0, here("data/processed_diagnoses/diagnoses_gpt4.0.csv.gz"))
# write_csv(df_claude3_haiku_t1.0, here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0.csv.gz"))
# write_csv(df_claude3_haiku_t0.1, here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1.csv.gz"))
# write_csv(df_claude3_opus_t1.0, here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0.csv.gz"))
# write_csv(df_gemini1.0_pro_t1.0, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))
# write_csv(df_gemini1.5_flash_t1.0, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))
# 
# 
# # Write unique diagnoses files
# 
# create_unique_diagnosis_file <- function(df, out_path){
#   df %>% 
#     # select(diagnosis) %>% 
#     # distinct() %>% 
#     count(diagnosis) %>% 
#     write_csv(out_path)
# }
# 
# create_unique_diagnosis_file(
#   df_gpt3.5,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_gpt3.5.csv")
# )
# create_unique_diagnosis_file(
#   df_gpt4.0,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_gpt4.0.csv")
# )
# create_unique_diagnosis_file(
#   df_claude3_haiku_t1.0,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_claude3_haiku_t1.0.csv")
# )
# create_unique_diagnosis_file(
#   df_claude3_opus_t1.0,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_claude3_opus_t1.0.csv")
# )
# create_unique_diagnosis_file(
#   df_gemini1.0_pro_t1.0,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_gemini1.0_pro_t1.0.csv")
# )
# 
# df_gpt3.5_raw <- process_json(here("data/chatgpt_json_output/gpt-3.5-turbo-1106/"), clean_method = "none")
# df_gpt4.0_raw <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"), clean_method = "none")
# df_claude3_haiku_t1.0_raw <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t1-0/"), clean_method = "none")
# df_claude3_haiku_t0.1_raw <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t0-1/"), clean_method = "none")
# df_claude3_opus_t1.0_raw <- process_json(here("data/claude_json_output/claude-3-opus-20240229_t1-0/"), clean_method = "none")
# df_gemini1.0_pro_t1.0_raw <- process_json(here("data/gemini_json_output/gemini-1.0-pro_t1-0/"), clean_method = "none")
# df_gemini1.5_flash_t1.0_raw <- process_json(here("data/gemini_json_output/gemini-1.5-flash-latest_t1-0/"), clean_method = "none")
# 
# 
# # Write analysis data files
# write_csv(df_gpt3.5_raw, here("data/processed_diagnoses/diagnoses_gpt3.5_raw.csv.gz"))
# write_csv(df_gpt4.0_raw, here("data/processed_diagnoses/diagnoses_gpt4.0_raw.csv.gz"))
# write_csv(df_claude3_haiku_t1.0_raw, here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0_raw.csv.gz"))
# write_csv(df_claude3_haiku_t0.1_raw, here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1_raw.csv.gz"))
# write_csv(df_claude3_opus_t1.0_raw, here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0_raw.csv.gz"))
# write_csv(df_gemini1.0_pro_t1.0_raw, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0_raw.csv.gz"))
# write_csv(df_gemini1.5_flash_t1.0_raw, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0_raw.csv.gz"))
# 
# create_unique_diagnosis_file(
#   df_gpt3.5_raw,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_gpt3.5_raw.csv")
# )
# create_unique_diagnosis_file(
#   df_gpt4.0_raw,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_gpt4.0_raw.csv")
# )
# create_unique_diagnosis_file(
#   df_claude3_haiku_t1.0_raw,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_claude3_haiku_t1.0_raw.csv")
# )
# create_unique_diagnosis_file(
#   df_claude3_opus_t1.0_raw,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_claude3_opus_t1.0_raw.csv")
# )
# create_unique_diagnosis_file(
#   df_gemini1.0_pro_t1.0_raw,
#   out_path = here("data/unique_diagnoses/unique_diagnoses_gemini1.0_pro_t1.0_raw.csv")
# )
# 
