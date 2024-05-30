library(here)
library(broom)
library(tidyverse)

icd_pca <- readRDS(here("data/chatgpt_embeddings/text-embedding-3-small/icd10_chatgpt_embedding_pca.RDS"))

max_distance <- function(n = 1000){
  m <- icd_pca$x[sample(1:nrow(icd_pca$x), size = n, replace = F), ]
  output <- dist(m) %>% 
    broom::tidy() %>% 
    slice_max(distance, n=1) %>% 
    unite(comp, item1, item2, sep = "-")
  
  write_csv(output, file = here("tally_max_distance.csv"), append = T, col_names = F)
}

max <- 10000
for (i in 1:max){
  print(str_glue("{i} of {max}"))
  max_distance()
}

