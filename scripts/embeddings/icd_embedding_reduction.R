# Read in data with all ICD code ChatGPT embeddings and perform PCA analysis
# Save PCA object so that can be used to predict loadings in same 
# PCA space for additional ChatGPT embeddings

library(tidyverse)
library(here)

types <- c(code = "c", diagnosis = "c", .default = "d")

print("READING DATA")
df <- vroom::vroom(
  here("data/chatgpt_embeddings/icd10_chatgpt_embeddings.csv.gz"),
  col_types = types,
  num_threads = 10)

print("FORMATTING DATA")
df <- df %>% 
  column_to_rownames("code") %>% 
  select(-diagnosis)

print("RUNNING PCA")
df %>% 
  prcomp() %>% 
  saveRDS(here("data/chatgpt_embeddings/icd10_chatgpt_embedding_pca.RDS"))

# print("RUNNING UMAP")
# set.seed(1234)
# df %>% 
#   umap::umap() %>% 
#   saveRDS(here("data/icd10_embedding_umap.RDS"))
