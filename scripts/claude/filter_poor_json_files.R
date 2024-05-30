# Original claude_diagnosis_iteration.py script had a function format_claude_json
# containing: `string_list = string[string.find('[')+1:string.find(']')].split(',')`
# The purpose of this was to flip the type of quotation mark (i.e. single vs. double)
# to make the string compatible wiht python's json parser

# However, this had the unintended consequence of splitting strings containing 
# commas, so 'hand, foot, and mouth disease, would become ['hand', 'foot', 'and mouth']

# The function was corrected with:
# `string_list = re.split('",|\',', string[string.find('[')+1:string.find(']')])`

# For less expensive models, the entire script was rerun to generate new data.
# However, Claude3 Opus was too expensive to re-run in its entirety

# This script specifically removes Claude3 Opus outputs from the original json
# files that contain responses with comma splitting 

# claude_diagnosis_iteration.py automatically detects absent iterations in the json
# files, so by removing comma-split data and rerunning claude_diagnosis_iteration.py,
# only the removed iterations will be re-queried. 

library(tidyverse)
library(here)
library(jsonlite)

in_dir <- "data/claude_json_output/archive/claude-3-opus-20240229_t1-0/"
out_dir <- "data/claude_json_output/claude-3-opus-20240229_t1-0/"

tibble(file = list.files(here(in_dir), full.names = F)) %>% 
  # head(3) %>%
  mutate(data = map(file, ~fromJSON(here(str_glue("{in_dir}/{.}")))))%>% 
  unnest(data) %>% 
  mutate(n_diag = map_dbl(diagnoses, length)) %>% 
  # Comma-split responses will have more than 10 diagnoses
  filter(n_diag <= 10) %>% 
  select(-n_diag) %>% 
  nest(.by = "file") %>% 
  mutate(data = map2(file, data, function(f, d){
    json <- toJSON(d, pretty = T)
    write(json, file = here(str_glue("{out_dir}/{f}")))
  }))
