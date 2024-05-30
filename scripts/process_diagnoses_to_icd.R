library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))

# map_diagnoses_to_icd <- function(df, diagnosis_model){
#   key_path <- str_glue("data/chatgpt_embeddings/text-embedding-3-small/{diagnosis_model}_diagnoses_chatgpt_embeddings_to_ICD.csv")
#   df_icd_key <- read_csv(here(key_path))
#   df <- df %>% 
#     left_join(df_icd_key, by = c("diagnosis" = "diagnosis.x")) %>% 
#     select(-diagnosis) %>% 
#     unite(diagnosis, code, diagnosis.y, sep = " ")
#   return(df)
# }


map_diagnoses_to_icd <- function(model){
  diagnoses_path <- here(str_glue("data/processed_diagnoses/diagnoses_{model}.csv.gz"))
  key_path <- here(str_glue("data/chatgpt_embeddings/text-embedding-3-small/{model}_diagnoses_chatgpt_embeddings_to_ICD.csv"))
  outpath_path <- here(str_glue("data/processed_diagnoses/diagnoses_{model}_icd.csv.gz"))
  
  print(str_glue("READING DATA - {model}"))
  df_diagnoses <- read_csv(diagnoses_path)
  df_key <- read_csv(key_path)
  
  print(str_glue("JOINING DATA - {model}"))
  df_diagnoses <- df_diagnoses %>% 
    left_join(df_key, by = c("diagnosis" = "diagnosis.x")) %>% 
    select(-diagnosis) %>% 
    unite(diagnosis, code, diagnosis.y, sep = " ")
  
  print(str_glue("WRITING DATA - {model}"))
  write_csv(df_diagnoses, outpath_path)
}

# map_diagnoses_to_icd("gpt-3.5-turbo-1106")
# map_diagnoses_to_icd("gpt-4-turbo-preview")
# map_diagnoses_to_icd("claude-3-haiku-20240307_t1-0")
# map_diagnoses_to_icd("claude-3-opus-20240229_t1-0")
# map_diagnoses_to_icd("gemini-1.0-pro-002_t1-0")
# map_diagnoses_to_icd("gemini-1.5-flash-preview-0514_t1-0")
map_diagnoses_to_icd("gemini-1.5-pro-001_t1-0")

# 
# df_gpt3.5 <- read_csv(here("data/processed_diagnoses/diagnoses_gpt3.5.csv.gz"))
# df_gpt4.0 <- read_csv(here("data/processed_diagnoses/diagnoses_gpt4.0.csv.gz"))
# df_claude3_haiku_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0.csv.gz"))
# # df_claude3_haiku_t0.1 <- read_csv(here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1.csv.gz"))
# df_claude3_opus_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0.csv.gz"))
# df_gemini1.0_pro_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))
# # df_gemini1.5_flash_t1.0 <- read_csv(here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0.csv.gz"))
# 
# 
# df_gpt4.0_icd <- map_diagnoses_to_icd(df_gpt4.0, 'gpt4.0')
# df_gpt3.5_icd <- map_diagnoses_to_icd(df_gpt3.5, 'gpt3.5')
# df_claude3_haiku_t1.0_icd <- map_diagnoses_to_icd(df_claude3_haiku_t1.0, 'claude3_haiku_t1.0')
# df_claude3_opus_t1.0_icd <- map_diagnoses_to_icd(df_claude3_opus_t1.0, 'claude3_opus_t1.0')
# df_gemini1.0_pro_t1.0_icd <- map_diagnoses_to_icd(df_claude3_opus_t1.0, 'gemini1.0_pro_t1.0')
# 
# 
# write_csv(df_gpt3.5_icd, here("data/processed_diagnoses/diagnoses_gpt3.5_icd.csv.gz"))
# write_csv(df_gpt4.0_icd, here("data/processed_diagnoses/diagnoses_gpt4.0_icd.csv.gz"))
# write_csv(df_claude3_haiku_t1.0_icd, here("data/processed_diagnoses/diagnoses_claude3_haiku_t1.0_icd.csv.gz"))
# # write_csv(df_claude3_haiku_t0.1_icd, here("data/processed_diagnoses/diagnoses_claude3_haiku_t0.1_icd.csv.gz"))
# write_csv(df_claude3_opus_t1.0_icd, here("data/processed_diagnoses/diagnoses_claude3_opus_t1.0_icd.csv.gz"))
# write_csv(df_gemini1.0_pro_t1.0_icd, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0_icd.csv.gz"))
# # write_csv(df_gemini1.5_flash_t1.0_icd, here("data/processed_diagnoses/diagnoses_gemini1.0_pro_t1.0_icd.csv.gz"))