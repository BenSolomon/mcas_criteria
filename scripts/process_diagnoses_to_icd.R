library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))

map_diagnoses_to_icd <- function(df, diagnosis_model){
  key_path <- str_glue("data/chatgpt_embeddings/text-embedding-3-small/{diagnosis_model}_diagnoses_chatgpt_embeddings_to_ICD.csv")
  df_icd_key <- read_csv(here(key_path))
  df <- df %>% 
    left_join(df_icd_key, by = c("diagnosis" = "diagnosis.x")) %>% 
    select(-diagnosis) %>% 
    unite(diagnosis, code, diagnosis.y, sep = " ")
  return(df)
}

df_gpt3.5 <- read_csv(here("data/processed_diagnoses/diagnoses_gpt3.5.csv.gz"))
df_gpt4.0 <- read_csv(here("data/processed_diagnoses/diagnoses_gpt4.0.csv.gz"))
df_claude3_haiku_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0.csv.gz"))
df_claude3_haiku_t0.1 <- read_csv(here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1.csv.gz"))
df_claude3_opus_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0.csv.gz"))
df_gemini1.0_pro_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))
df_gemini1.5_flash_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))


df_gpt4.0_icd <- map_diagnoses_to_icd(df_gpt4.0, 'gpt4.0')
df_gpt3.5_icd <- map_diagnoses_to_icd(df_gpt3.5, 'gpt3.5')
df_claude3_haiku_t1.0_icd <- map_diagnoses_to_icd(df_claude3_haiku_t1.0, 'claude3_haiku_t1.0')


write_csv(df_gpt3.5_icd, here("data/processed_diagnoses/diagnoses_gpt3.5_icd.csv.gz"))
write_csv(df_gpt4.0_icd, here("data/processed_diagnoses/diagnoses_gpt4.0_icd.csv.gz"))
write_csv(df_claude3_haiku_t1.0_icd, here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0_icd.csv.gz"))
# write_csv(df_claude3_haiku_t0.1_icd, here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1_icd.csv.gz"))
# write_csv(df_claude3_opus_t1.0_icd, here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0_icd.csv.gz"))
# write_csv(df_gemini1.0_pro_t1.0_icd, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0_icd.csv.gz"))
# write_csv(df_gemini1.5_flash_t1.0_icd, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0_icd.csv.gz"))