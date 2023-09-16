# Clean a diagnosis from a chatGPT output, removing list numbers, bullet points, 
# removes white space, and converts to lower case to improve standardization
clean_diagnosis <- function(diagnosis){
  diagnosis <- str_extract(diagnosis, "[a-zA-Z].*") # Remove all non-characters from beginning (e.g. "1)", " - ")
  diagnosis <- tolower(diagnosis) 
  diagnosis <- gsub("\\([a-z]*\\)","", diagnosis)
  diagnosis <- gsub("\\.","", diagnosis)
  diagnosis <- trimws(diagnosis)
  diagnosis <- diagnosis[!grepl(":$", diagnosis)] # Remove lines like "The top diagnoses are:"
  diagnosis <- diagnosis[!is.na(diagnosis)] # Remove NAs
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