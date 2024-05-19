require(tidyverse)
require(jsonlite)
require(here)
require(future)
require(future.apply)
require(parallelDist)
require(vegan)
require(checkmate)

source(here("utils/general.R"))

######################### READING AND CLAEANING DATA ###########################
check_valid_diagnoses <- function(diagnoses){
  if (is.null(diagnoses)){return(FALSE)}
  if (diagnoses[1]=="error"){return(FALSE)}
  return(TRUE)
}

# Check if ChatGPT output was a valid JSON structure and remove if not
# TODO Could be refined
check_valid_json <- function(json) {
  invalid <- json %>%
    # mutate(invalid_diagnoses = map(diagnoses, is.null)) %>%
    mutate(invalid_diagnoses = map_lgl(diagnoses, ~!check_valid_diagnoses(.))) %>% 
    filter(invalid_diagnoses == T) %>%
    unite(iteration_output, i, criteria, sep = "_") %>%
    pull(iteration_output) %>% 
    paste(collapse = ", ")
  print(sprintf("Invalid iterations: %s", invalid))
  json %>%
    mutate(invalid_diagnoses = map_lgl(diagnoses, ~!check_valid_diagnoses(.))) %>%
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
### LEGACY - Used prior to introducing knn-matching to ICD codes. Focused more
### on generating parsimonious responses (e.g. removing modifiers like 'unspecified')
### Results were less consistent when applied across responses from different
### LLM models

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
# Identifies strings that represent a concatenated lists and separates them

# Generalized function to find list-strings based on patter and split them 
# based on a second patter. 
# perl_method must be TRUE is look-forwards/backs are needed
unnest_pattern <- function(df, search_pattern, split_pattern, perl_method = F){
  df %>% 
    filter(grepl(search_pattern, diagnosis, perl = perl_method)) %>% 
    mutate(diagnosis = str_split(diagnosis, split_pattern)) %>% 
    unnest(diagnosis) %>% 
    select(i, criteria, diagnosis)
}

# Function that unifies multiple unnest_pattern calls to assemble final df
unnest_oneliners <- function(df){
  # Find all strings that contain all three of 1. 2. 3. (and likely more) and split them apart
  number_pattern <- "^(?=.*1\\.)(?=.*2\\.)(?=.*3\\.).+$"
  letter_pattern <- "^(?=.*a\\.)(?=.*b\\.)(?=.*c\\.).+$"
  html_pattern <- '<.{2,3}>'
  newline_pattern <- "(.*\\n){5}"
  
  
  oneliner_numbers <- unnest_pattern(df, number_pattern, "[0-9]{1,2}\\.", perl_method = T)
  oneliner_letters <- unnest_pattern(df, letter_pattern, "[a-l]{1}\\.", perl_method = T)
  oneliner_html <- unnest_pattern(df, html_pattern, html_pattern, perl_method = F)
  oneliner_newline <- unnest_pattern(df, newline_pattern, "\\n", perl_method = T)
  
  
  not_oneliners <- df %>% 
    filter(!grepl(number_pattern, diagnosis, perl = T)) %>% 
    filter(!grepl(letter_pattern, diagnosis, perl = T)) %>% 
    filter(!grepl(newline_pattern, diagnosis, perl = T)) %>% 
    filter(!grepl(html_pattern, diagnosis)) 
  
  bind_rows(
    not_oneliners,
    oneliner_numbers,
    oneliner_letters,
    oneliner_html,
    oneliner_newline
  )
}

################################################################################
# Converts all strings to ASCII and removes any string with characters
# that could not be converted to a compatible character
# E.g. will remove non-roman alphabet language strings 
remove_non_ascii <- function(df){
  df %>% 
    mutate(diagnosis = iconv(diagnosis, to="ASCII//TRANSLIT")) %>% 
    filter(!grepl("\\?", diagnosis)) %>% 
    select(i, criteria, diagnosis)
}
################################################################################
# Function to clean responses generate by LLM in a variety of ways such as:
## Eliminating line breaks, json formatting, list prefixes, white space,
## non ASCII characters, non-diagnosis strings, and more

clean_responses <- function(df){
  
  # To remove line breaks or characters that might be confused for json formatting
  special_patterns <- c("\\\\n", "\\\\|", "\\{", "\\}", "\\[", "\\]")
  
  # To filter responses where LLM desclined to give an answer
  llm_patterns <- c("qualified", "sorry", "provide")
  
  # To filter elements of a response that do not represent the diagnosis itself
  filter_patterns <- c(llm_patterns,
                       "description:",
                       "explanation:",
                       "id:",
                       "icd10:",
                       "prevalence",
                       "probability:",
                       "details:",
                       "info:",
                       "reason:",
                       "notes:",
                       "code:",
                       "confidence:",
                       "count:",
                       "definition:",
                       "diagnostic category:",
                       "diagnostic criteria:",
                       "frequency:",
                       "likelihood:",
                       "snomed-ct:",
                       "symptoms:",
                       "details:",
                       "true",
                       "false",
                       "score:",
                       "\\$[0-9]", # Currency string
                       "^and ", # Vast majority of responses starting with 'and ' are restating the query symptoms
                       "^can " # Vast majority of responses starting with 'can ' are restating the query symptoms
  )
  
  # To remove extraneous characters in a response containing a diagnosis
  trim_patterns <- c("name:",
                     "diagnosis:",
                     "diagnoses:",
                     "term:",
                     "text:",
                     "condition:",
                     "\\*",
                     "\\\\\\u2019s", # Unicode apostrophe
                     "#",
                     "\\$",
                     "<.*>" # Remove urls
  )
  
  df %>% 
    mutate(diagnosis = tolower(diagnosis)) %>% # Make all lowercase
    unnest_oneliners() %>% # Split strings w/ multiple diagnoses
    mutate(diagnosis = gsub(paste(special_patterns, collapse = "|"), "", diagnosis)) %>% # Remove special patterns
    # mutate(diagnosis = gsub("(\\{|\\}|\\[|\\])","", diagnosis)) %>% # Remove brackets
    remove_non_ascii() %>%  # Remove strings that cant be converted to ASCII
    filter(!grepl(paste(filter_patterns, collapse = "|"), diagnosis)) %>% 
    mutate(diagnosis = gsub(paste(trim_patterns, collapse = "|"), "", diagnosis)) %>%
    mutate(diagnosis = gsub("^[^a-zA-Z]*(?=[a-zA-Z])", "", diagnosis, perl = TRUE)) %>% # Remove all characters until the first letter (e.g numbered lists)
    mutate(diagnosis = gsub("_"," ",diagnosis)) %>% 
    mutate(diagnosis = gsub(" - .*","",diagnosis)) %>% #Vast majority with " - " are of the form "{diagnosis} - {description}"
    filter(!grepl("^(icd|http)", diagnosis)) %>% 
    mutate(diagnosis = trimws(diagnosis)) %>% 
    filter(str_count(diagnosis, "[[:alpha:]]") > 2) %>% # Remove strings with less than 3 letters
    filter(grepl("[a-z]", diagnosis)) %>% # Remove strings with no letters
    filter(diagnosis != "") # Remove empty strings
}

################################################################################

# Pipeline for importing and processing all json data from files in a directory
process_json <- function(json_dir, clean_method = "current"){
  # Input checks
  arg_col <- makeAssertCollection()
  assertChoice(clean_method, c("legacy", "current", "none"), add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}

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
    drop_na() 
  
  if (clean_method == "legacy"){df <- test_clean(df)}
  if (clean_method == "current"){df <- clean_responses(df)}

  print(sprintf("Iterations processed: %s", length(unique(df$i))))
  return(df)
}

############################# DATA CALCULATIONS ################################

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

################################################################################

# Calculate diagnosis diversity for all criteria and then calculate 
# difference in diversity between all pairs of categories
pairwise_diversity_difference <- function(df){
  df_div <- enframe(vegan::diversity(table(df$criteria, df$diagnosis)))
  df_diff <- data.frame(t(combn(unique(sort(df$criteria)),2))) %>% 
    left_join(df_div, by = c("X1"="name")) %>% 
    left_join(df_div, by = c("X2"="name")) %>% 
    unite("pair", X1, X2, sep = ".") %>%
    mutate(diversity_difference = value.x - value.y) %>% 
    select(-contains("value"))
  deframe(df_diff)
}


################################################################################

# Important for precision because there will be choose(gpt_iterations, 2)
# number of bray-curtis calculations
limit_gpt_iterations <- function(df, gpt_iterations){
  df %>%  
    mutate(i = as.numeric(i)) %>% 
    filter(i <= gpt_iterations)
}

################################################################################

# Calculate bray-curtis distance between all gpt_iteration results of a given criteria 
calculate_precision <- function(df, gpt_iterations = NULL){
  # If not null, limit df to number of gpt_iterations specified
  # There will be choose(gpt_iterations, 2) number of bray-curtis calculations
  if (!is.null(gpt_iterations)){df <- limit_gpt_iterations(df, gpt_iterations)}
  
  df %>%  
    nest(.by = criteria) %>% 
    mutate(data = map(data, function(df){
      diagnosis_table <- table(df$i, df$diagnosis)
      # parDist will use maximum threads available on system
      dist_mtx <- parallelDist::parDist(diagnosis_table, method = "bray")
      broom::tidy(dist_mtx) %>% 
        select(distance)
    })) %>% 
    unnest(data)
}

################################################################################

# Calculate diagnosis metric for all criteria and then calculate
# difference in that metric between all pairs of categories
pairwise_distance_difference <- function(df, metric, gpt_iterations=NULL){
  if (!is.null(gpt_iterations)){df <- limit_gpt_iterations(df, gpt_iterations)}
  
  if (metric == "diversity"){
    df <- enframe(vegan::diversity(table(df$criteria, df$diagnosis)),
                  "criteria", "distance")
  }
  if (metric == "precision"){
    df <- calculate_precision(df) %>% 
      summarise(distance = mean(distance), .by = criteria)
  }
  
  data.frame(t(combn(unique(sort(df$criteria)),2))) %>% 
    left_join(df, by = c("X1"="criteria")) %>% 
    left_join(df, by = c("X2"="criteria")) %>% 
    unite("pair", X1, X2, sep = ".") %>% 
    mutate(distance_difference = distance.x - distance.y) %>% 
    select(pair, distance_difference) %>% 
    deframe()
}

################################################################################

# Perform a single data permutation for percision calculations 
# and then calculate the precision difference between all categories
permute_precision_iteration <- function(df){
  df %>%
    mutate(diagnosis = sample(diagnosis, size = n(), replace = F), .by = criteria) %>%
    pairwise_distance_difference(metric = "precision")
}

################################################################################

# Perform a single data permutation for diversity calculations 
# and then calculate the diversity difference between all categories
permute_diversity_iteration <- function(df){
  out <- as.data.frame(t(combn(unique(sort(df$criteria)),2))) %>% 
    mutate(data = map2(V1, V2, function(v1,v2){
      df %>% 
        ungroup() %>% 
        filter(criteria == v1 | criteria == v2) %>% 
        mutate(diagnosis = sample(diagnosis, replace = F))})) %>% 
    select(-contains("V")) %>% 
    unnest(data) %>% 
    pairwise_distance_difference(metric = "diversity")
  return(out)
}

################################################################################

# Calculate the p-values for diversity or precision differences between criteria
# based on their # percentile relative to the null hypothesis permutation 
# distribution of those differences 
## gpt_iterations is effectively a filter of the number of original chatgpt
## iterations that are included. Fewer means faster calculation, but less accurate
difference_permutation_test <- function(df, metric, permutations = 10, gpt_iterations = NULL){
  if (!is.null(gpt_iterations)){df <- limit_gpt_iterations(df, gpt_iterations)}
  
  
  if (metric == "diversity"){
    plan(multisession)
    x <- future_replicate(n = permutations, permute_diversity_iteration(df), future.seed = 1234)
  }
  
  if (metric == "precision"){
    set.seed(1234)
    x <- replicate(n = permutations, permute_precision_iteration(df))
  }
  
  # Since order of criteria A vs. criteria B doesn't matter, positive vs. negative
  # difference value don't matter and would cancel each other out, so statistics
  # are based on the absolute values of the differences
  as.data.frame(x) %>% 
    rownames_to_column("pair") %>% 
    pivot_longer(!pair, names_to = "replicate", values_to = "difference") %>% 
    nest(.by = pair) %>% 
    mutate(null_ecdf = map(data, ~ecdf(abs(.$difference)))) %>% 
    mutate(null_mean = map_dbl(data, ~mean(abs(.$difference)))) %>% 
    mutate(quant = map(data, ~quantile(abs(.$difference), c(0.025,0.5,0.975)))) %>% 
    unnest_wider(quant) %>% 
    left_join(
      enframe(pairwise_distance_difference(df, metric = metric), 
              "pair", "difference"), by = c("pair")
    ) %>% 
    mutate(difference = abs(difference)) %>% 
    mutate(p_value = map2_dbl(null_ecdf, difference, function(e,m){1-e(m)})) %>% 
    mutate(p_value_adj = p.adjust(p_value, method = "BH")) %>% 
    select(-null_ecdf)
}

################################################################################
# Many calculations in study involve comparisons of pairs where one element of 
# the pair is in one column and the other is in a second column. However, the
# order of this pair may not matter, but affect the sign of the associated 
# calculation (e.g. difference). If which element of the pair ends up in the 
# first column is not consistent, this can result in discrepancies in the 
# associated calculation. This function ensures that the element in the first
# column is always the first in alphabetical order. 
sort_paired_columns <- function(df, prefix){
  prefix1 <- sprintf("%s1", prefix)
  prefix2 <- sprintf("%s2", prefix)
  
  df %>% 
    mutate_at(c(prefix1, prefix2), as.character) %>% 
    mutate(!!prefix := map2(!!sym(prefix1),!!sym(prefix2), ~sort(c(.x,.y)))) %>% 
    select(-!!sym(prefix1), -!!sym(prefix2)) %>% 
    unnest_wider(prefix, names_sep = "")
}

