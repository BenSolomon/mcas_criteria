# Parse JSON files from ChatGPT to generate quick-to-load
# csvs of ChatGPT response

library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))

df_gpt3 <- process_json(here("data/chatgpt_json_output/gpt-3.5-turbo-1106/"))
df_gpt4 <- process_json(here("data/chatgpt_json_output/gpt-4-turbo-preview/"))
df_claude3_haiku_t1_0 <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t1-0/"))
df_claude3_haiku_t0_1 <- process_json(here("data/claude_json_output/claude-3-haiku-20240307_t0-1/"))
df_claude3_opus_t1_0 <- process_json(here("data/claude_json_output/claude-3-opus-20240229_t1-0/"))
df_gemini_t1_0 <- process_json(here("data/gemini_json_output/gemini-1.0-pro_t1-0/"))

df_icd_key <- read_csv(here("data/chatgpt_embeddings/diagnosis_chatgpt_embeddings_to_ICD.csv"))

df_gpt4_icd <- df_gpt4 %>% 
  left_join(df_icd_key, by = c("diagnosis" = "diagnosis.x")) %>% 
  select(-diagnosis) %>% 
  unite(diagnosis, code, diagnosis.y, sep = " ")

write_csv(df_gpt3, here("data/processed_diagnoses/diagnoses_gpt3.csv.gz"))
write_csv(df_gpt4, here("data/processed_diagnoses/diagnoses_gpt4.csv.gz"))
write_csv(df_gpt4_icd, here("data/processed_diagnoses/diagnoses_gpt4_icd.csv.gz"))
write_csv(df_claude3_haiku_t1_0, here("data/processed_diagnoses/diagnoses_claude3_haiku_t1-0.csv.gz"))
write_csv(df_claude3_haiku_t0_1, here("data/processed_diagnoses/diagnoses_claude3_haiku_t0-1.csv.gz"))
write_csv(df_claude3_opus_t1_0, here("data/processed_diagnoses/diagnoses_claude3_opus_t1-0.csv.gz"))
write_csv(df_gemini_t1_0, here("data/processed_diagnoses/diagnoses_gemini_t1-0.csv.gz"))

