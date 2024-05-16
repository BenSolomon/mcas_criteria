# Parse JSON files from ChatGPT to generate quick-to-load
# csvs of ChatGPT response

library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))

df_gpt3.5 <- process_json(here("data/chatgpt_json_output/gpt-3.5-turbo-1106/"))
df_gpt4.0 <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))
df_claude3_haiku_t1.0 <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t1-0/"))
df_claude3_haiku_t0.1 <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t0-1/"))
df_claude3_opus_t1.0 <- process_json(here("data/claude_json_output/claude-3-opus-20240229_t1-0/"))
df_gemini1.0_pro_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.0-pro_t1-0/"))
df_gemini1.5_flash_t1.0 <- process_json(here("data/gemini_json_output/gemini-1.5-flash-latest_t1-0/"))


# Write analysis data files
write_csv(df_gpt3.5, here("data/processed_diagnoses/diagnoses_gpt3.5.csv.gz"))
write_csv(df_gpt4.0, here("data/processed_diagnoses/diagnoses_gpt4.0.csv.gz"))
write_csv(df_claude3_haiku_t1.0, here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0.csv.gz"))
write_csv(df_claude3_haiku_t0.1, here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1.csv.gz"))
write_csv(df_claude3_opus_t1.0, here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0.csv.gz"))
write_csv(df_gemini1.0_pro_t1.0, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))
write_csv(df_gemini1.5_flash_t1.0, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))


# Write unique diagnoses files

create_unique_diagnosis_file <- function(df, out_path){
  df %>% 
    select(diagnosis) %>% 
    distinct() %>% 
    write_csv(out_path)
}

create_unique_diagnosis_file(
  df_gpt3.5,
  out_path = here("data/unique_diagnoses/unique_diagnoses_gpt3.5.csv")
)
create_unique_diagnosis_file(
  df_gpt4.0,
  out_path = here("data/unique_diagnoses/unique_diagnoses_gpt4.0.csv")
)
create_unique_diagnosis_file(
  df_claude3_haiku_t1.0,
  out_path = here("data/unique_diagnoses/unique_diagnoses_claude3_haiku_t1.0.csv")
)
create_unique_diagnosis_file(
  df_claude3_opus_t1_0,
  out_path = here("data/unique_diagnoses/unique_diagnoses_claude3_opus_t1.0.csv")
)
create_unique_diagnosis_file(
  df_gemini1.0_pro_t1.0,
  out_path = here("data/unique_diagnoses/unique_diagnoses_gemini1.0_pro_t1.0.csv")
)
