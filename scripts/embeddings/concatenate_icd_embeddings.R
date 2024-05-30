library(tidyverse)
library(here)

compile_csv <- function(dir_path, out_path, threads = 1){
  types <- c(code = "c", diagnosis = "c", .default = "d")
  
  files <- list.files(dir_path, full.names = T)
  if (file.exists(out_path)){file.remove(out_path)} # Remove pre-existing file
  for (f in files){
    print(sprintf("READING FILE: %s", f))
    df <- vroom::vroom(f, num_threads = threads, col_types = types)
    # For first iteration, create new file, include column names
    if (!file.exists(out_path)){
      vroom::vroom_write(df, out_path, delim = ",", append = F, col_names = T, num_threads = threads)
    # For all subsequent iterations, append to previous file, without column names
    } else {
      vroom::vroom_write(df, out_path, delim = ",", append = T, col_names = F, num_threads = threads)
    }
  }
}

compile_csv(dir_path = here("data/chatgpt_embeddings/icd_embeddings/"), out_path = here("data/chatgpt_embeddings/icd10_chatgpt_embeddings.csv.gz"))