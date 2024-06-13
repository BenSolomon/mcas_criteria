library(tidyverse)
library(here)
library(RANN)

model <- "text-embedding-3-small"
limit <- 1.485715

print("READING DATA")

# Read in ICD code PCA reduction
icd_pca <- readRDS(here(str_glue("data/chatgpt_embeddings/{model}/icd10_chatgpt_embedding_pca.RDS")))

# Read in ICD key
df_icd_key <- vroom::vroom( 
  here(str_glue("data/chatgpt_embeddings/{model}/icd10_chatgpt_embeddings.csv.gz")), 
  col_select = c("code", "diagnosis"),
  col_types = c(.default = "c"))

create_diagnosis_to_ICD_mapping <- function(diagnosis_data){
  embedding_path <- str_glue("data/chatgpt_embeddings/{model}/{diagnosis_data}_diagnoses_chatgpt_embeddings.csv.gz")
  output_path <- str_glue("data/chatgpt_embeddings/{model}/{diagnosis_data}_diagnoses_chatgpt_embeddings_to_ICD.csv")
  
  # Read in diagnosis embeddings
  df <- vroom::vroom(
    here(embedding_path), 
    # n_max = 100,
    col_types = c(diagnosis = "c", .default = "d"))
  
  # Give each diagnosis an index
  df <- df %>% mutate(index = 1:n())
  
  # Create an index key 
  df_key <- df %>% select(index, diagnosis)
  
  # Format embeddings for reduction
  df <- df %>% 
    select(-diagnosis) %>% 
    column_to_rownames("index")
  
  print(str_glue("APPLYING PCA REDUCTION - {diagnosis_data}"))
  # Apply ICD code PCA reduction to diagnosis embeddings
  gpt_pca <- predict(icd_pca, df)
  
  print(str_glue("FINDING NEAREST NEIGHBORS - {diagnosis_data}"))
  # Perform nearest neighbor search 
  nn_results <- RANN::nn2(icd_pca$x, gpt_pca, k = 1, radius = limit, searchtype = "radius")
  
  print(str_glue("WRITING DATA - {diagnosis_data}"))
  df_key %>% 
    mutate(code = rownames(icd_pca$x)[nn_results$nn.idx[,1]]) %>% 
    left_join(df_icd_key, by = "code") %>% 
    write_csv(here(output_path))
  
  # saveRDS(list(nn_results, df_key, icd_pca$x, df_icd_key, gpt_pca), here(str_glue("{output_path}.RDS")))
}

create_diagnosis_to_ICD_mapping("gpt-3.5-turbo-1106")
create_diagnosis_to_ICD_mapping("gpt-4-turbo-preview")
create_diagnosis_to_ICD_mapping("claude-3-haiku-20240307_t1-0")
create_diagnosis_to_ICD_mapping("claude-3-opus-20240229_t1-0")
create_diagnosis_to_ICD_mapping("gemini-1.0-pro-002_t1-0")
create_diagnosis_to_ICD_mapping("gemini-1.5-flash-preview-0514_t1-0")
create_diagnosis_to_ICD_mapping("gemini-1.5-pro-001_t1-0")

