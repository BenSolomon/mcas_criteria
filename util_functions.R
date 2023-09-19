# Clean a diagnosis from a chatGPT output, removing list numbers, bullet points, 
# removes non-specific strings,
# removes white space, and converts to lower case to improve standardization
clean_diagnosis <- function(diagnosis){
  # Terms that modify many diagnosis that can be removed to consolidate variation
  patterns_to_remove <- c(
    "other unspecified",
    ", unspecified",
    "other specified",
    "with.*",
    ".*due to",
    ".*secondary to",
    " not .*",
    "^a "
    # "with normal pulmonary function tests",
    # "without abnormal pulmonary function tests",
    # "not otherwise specified",
    # "due to other specified causes",
    # "due to other specified bacterial agents",
    # "due to other specified non tuberculous bacterial agents",
    # "due to other specified infectious agents",
    # "with organ or system involvement",
    # "with other organ involvement",
    # "with organ involvement",
    # "without organ involvement",
    # "without organ or system involvement",
  )
  
  # Terms that are indicative of a "diagnosis" that should be removed
  # e.g. "it is crucial to consult with a healthcare professional"
  diagnoses_to_remove <- c(
    "language model",
    "healthcare professional",
    "medical attention",
    "medical professional"
  )
  
  diagnosis <- str_extract(diagnosis, "[a-zA-Z].*") # Remove all non-characters from beginning (e.g. "1)", " - ")
  diagnosis <- tolower(diagnosis) 
  diagnosis <- gsub("\\([a-z]*\\)","", diagnosis) # Remove lettered bullet points
  diagnosis <- gsub("\\.","", diagnosis)
  diagnosis <- gsub("\\-"," ", diagnosis) # ChatGPT inconsistently hyphenates
  diagnosis <- gsub("\\(.*\\)", "", diagnosis) # Remove any parenthetical like (e.g. x, y, z)
  diagnosis <- gsub(paste(patterns_to_remove, collapse = "|"),"",diagnosis)
  diagnosis <- gsub("\\,|\\/", " ", diagnosis) # Remove , \
  diagnosis <- iconv(diagnosis, to="ASCII//TRANSLIT") # Remove accents
  diagnosis <- trimws(diagnosis) 
  diagnosis <- gsub(" +", " ", diagnosis) # Convert 2 or more spaces in a row to one
  diagnosis <- diagnosis[!grepl(":", diagnosis)] # Remove lines that contain colons like "The top diagnoses are:"
  diagnosis <- diagnosis[!grepl(paste(diagnoses_to_remove, collapse = "|"), diagnosis)] # Remove diagnoses_to_remove
  diagnosis <- diagnosis[!is.na(diagnosis)] # Remove NAs
  diagnosis <- diagnosis[nchar(diagnosis) != 0] # Remove ""
  return(diagnosis)
}

# Takes the list of diagnoses from a single chatGPT query output,
# converts it into a vector, and applies the cleaning function
process_diagnoses <- function(resp){
  diagnoses <- str_split(resp, "\n") %>% 
    unlist() %>% 
    clean_diagnosis() 
  return(diagnoses)
}

# Takes dataframe of diagnoses and consolidates similar diagnosis names
# based on clustering key generated from /scripts/consolidate_similar_diagnoses.R
consolidate_diagnoses <- function(df){
  df_key <- read_csv(here("data/diagnosis_consolidation_key.csv"))
  df %>% 
    left_join(df_key, by = "diagnosis") %>% 
    mutate(diagnosis = ifelse(is.na(cluster_name), diagnosis, cluster_name)) %>% 
    select(-cluster_name)
}
