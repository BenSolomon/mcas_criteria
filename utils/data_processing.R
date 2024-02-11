require(tidyverse)
library(jsonlite)
library(here)

# Check if ChatGPT output was a valid JSON structure and remove if not
# TODO Could be refined
check_valid_json <- function(json) {
  invalid <- json %>%
    mutate(invalid_diagnoses = map(diagnoses, is.null)) %>%
    filter(invalid_diagnoses == T) %>%
    unite(iteration_output, i, criteria, sep = "_") %>%
    pull(iteration_output) %>% 
    paste(collapse = ", ")
  print(sprintf("Invalid iterations: %s", invalid))
  json %>%
    mutate(invalid_diagnoses = map(diagnoses, is.null)) %>%
    filter(invalid_diagnoses == F) %>%
    select(i, criteria, symptoms, diagnoses)
}
# fromJSON("data/chatgpt_json_output/gpt-4-turbo-preview//chatgpt_output_20240207_070955.json") %>% 
#   check_valid_json()

################################################################################

# Some GPT diagnosis response are actually lists of the diagnosis and a description of the diagnosis
# For those, this function extracts the disease name and reproduces the intended json structure
fix_diagnoses_json <- function(json_input){
  if (class(json_input) == "character"){
    return(json_input)
  } else if (class(json_input) == "data.frame"){
    return(json_input$name)
  }
}

################################################################################

# Consolidate diagnoses the are similar except for a specific string
# E.g. "DRESS syndrome" -> "DRESS" when "DRESS" also occurs in the data
# but not "Turner syndrome" -/-> "Turner" since "Turner" doesn't occur in data
consolidate_prefix <- function(df, string){
  
  # Find all diagnoses matching string and create key w_ and wo_ string removed
  prefix_df <- df %>% 
    count(w_suffix = diagnosis, sort = T) %>% 
    filter(grepl(string, w_suffix)) %>% 
    mutate(wo_suffix = trimws(gsub(string, "", w_suffix))) %>% 
    select(-n)
  
  # Match key to df to find if diagnosis wo_ string exist in dataset
  prefix_df <- df %>% 
    count(diagnosis, sort = T) %>% 
    left_join(prefix_df, by = c("diagnosis" = "wo_suffix")) %>% 
    rename(wo_suffix = diagnosis) %>% 
    drop_na() %>%
    select(-n)
  
  # For any diagnoses where w_ and wo_ string exist, consolidate to wo_string
  df %>% 
    left_join(prefix_df, by = c("diagnosis" = "w_suffix")) %>% 
    mutate(diagnosis = ifelse(
      is.na(wo_suffix),
      diagnosis,
      wo_suffix
    )) %>% 
    select(-wo_suffix)
}

################################################################################

# Function that cleans up various aspects of the diagnosis text that ChatGPT might generate
# TODO Rename
test_clean <- function(df){
  patterns_to_remove <- c(
    "other unspecified",
    ", unspecified",
    "other specified",
    # "with.*", #was excluding DRESS and similar
    ".*due to", #urticaria  due to allergic reaction -> allergic reaction
    ".*secondary to", #visual aura secondary to epilepsy -> epilepsy
    " not .*", #angioedema not otherwise specified -> angioedema
    "^a "
  )
  
  diagnoses_to_remove <- c(
    "language model",
    "healthcare professional",
    "medical attention",
    "medical professional"
  )
  
  df <- df %>%
    mutate(diagnosis = str_extract(diagnosis, "[a-zA-Z].*")) %>% # Remove all non-characters from beginning (e.g. "1)", " - ")
    mutate(diagnosis = tolower(diagnosis)) %>% 
    mutate(diagnosis = gsub(" +", " ", diagnosis)) %>% # Convert 2 or more spaces in a row to one
    mutate(diagnosis = gsub("\\([a-z]*\\)","", diagnosis) ) %>% # Remove lettered bullet points
    mutate(diagnosis = gsub("\\.","", diagnosis) )  %>% 
    mutate(diagnosis = gsub("\\(.*\\)", "", diagnosis) ) %>% # Remove any parenthetical like (e.g. x, y, z)
    mutate(diagnosis = gsub(" (-|–) .*", "", diagnosis) ) %>% # Remove hyphenated descriptions or rationales (e.g. zinc deficiency - could lead to follicular hyperkeratosis)
    mutate(diagnosis = gsub(" */ *","/", diagnosis) ) %>% 
    mutate(diagnosis = gsub("\\,", "", diagnosis) ) %>% #Remove comma
    mutate(diagnosis = iconv(diagnosis, to="ASCII//TRANSLIT")) %>% # Remove accents
    # Terms that modify many diagnosis that can be removed to consolidate variation
    mutate(diagnosis = gsub(paste(patterns_to_remove, collapse = "|"),"",diagnosis)) %>% 
    mutate(diagnosis = trimws(diagnosis))
  
  df %>% 
    filter(!grepl(paste(diagnoses_to_remove, collapse = "|"), diagnosis)) %>% 
    filter(!nchar(diagnosis) == 0) %>% 
    drop_na()  %>% 
    consolidate_prefix(" syndrome$") %>% 
    consolidate_prefix(" disease$")
  
  
}

################################################################################

# Pipeline for importing and processing all json data from files in a directory
process_json <- function(json_dir){
  # browser()
  # Read in all json containing files
  print("Reading data")
  df <- tibble(file = list.files(json_dir, full.names = T)) %>%
    mutate(data = map(file, fromJSON)) %>% 
    select(data) %>% 
    mutate(data = map(data, ~select(., i:diagnoses))) %>% 
    unnest(data)
  df <- df %>% 
    check_valid_json() %>% 
    select(-symptoms) 
  
  duplicates <- df %>% count(criteria, i, sort = T) %>% filter(n>1) %>% unite(output, criteria, i) %>% pull(output)
  if (length(duplicates) == 0){
    duplicates <- "None"
  } else {
    duplicates <- paste(duplicates, collapse = ", ")
  }
  print(sprintf("Duplicates: %s", duplicates))
  
  print("Processing diagnoses")
  df <- df %>%
    mutate(diagnoses = map(diagnoses, fix_diagnoses_json)) %>% 
    unnest(diagnoses) %>% 
    rename(diagnosis = diagnoses) %>% 
    filter(diagnosis != "") %>%  
    drop_na() %>% # Check on what this is dropping
    test_clean()
  
  print(sprintf("Iterations processed: %s", length(unique(df$i))))
  return(df)
}

################################################################################

# Creates a dataframe that tallies the number of times two diagnoses appear 
# in the same differential diagnosis iteration
# Also obtains the frequency of that co-occurence and ranks them by frequency
create_codiagnosis_df <- function(df){
  df %>% 
    mutate(criteria = factor(criteria, levels = c("mcas_consortium", "mcas_alternative", "eular_acr_sle", "slicc_sle"))) %>% 
    filter(grepl("mcas|sle", criteria)) %>% 
    group_by(i, criteria) %>% 
    nest() %>% 
    mutate(len = map_dbl(data, nrow)) %>% 
    filter(len > 2) %>% 
    mutate(data = map(data, function(x) {
      try(as.data.frame(t(combn(sort(x$diagnosis), 2))))
    })) %>% 
    unnest(data) %>% 
    ungroup() %>% 
    count(criteria, V1, V2, sort = T) %>% 
    filter(V1 != V2) %>% 
    # Ensure that same V1, V2, order is used
    rowwise() %>% 
    mutate(from = min(V1, V2), to = max(V1, V2)) %>% 
    ungroup() %>% 
    summarise(n = sum(n), .by = c(criteria, from, to)) %>% 
    # Add some statistics
    group_by(criteria) %>%
    arrange(desc(n), .by_group = T) %>% 
    mutate(rank = 1:n())  %>% 
    mutate(freq = n/sum(n)) %>%
    select(from, to, n, criteria, rank, freq)
}

################################################################################

# Generate graphs from codiagnosis dataframe. 
# Option to limit to n_diagnoses number of diagnoses
make_codiagnosis_graph <- function(df, n_diagnoses = NULL){
  # If specified, limit to top diagnoses
  if (!is.null(n_diagnoses)){
    df <- df %>% 
      filter(rank <= top_n)
  }
  df %>% 
    as_tbl_graph() %>% 
    activate(edges) %>% 
    mutate(criteria = factor(criteria, levels = c("mcas_consortium", "mcas_alternative", "eular_acr_sle", "slicc_sle")))
}