library(tidyverse)
library(here)
library(jsonlite)

original_dir <- "data/claude_json_output/archive/claude-3-opus-20240229_t1-0/"
new_dir <- "data/claude_json_output/claude-3-opus-20240229_t1-0/"

new_samples <- tibble(file = list.files(here(new_dir), full.names = F)) %>% 
  filter(grepl("20240525", file)) %>% 
  mutate(data = map(file, ~fromJSON(here(str_glue("{new_dir}/{.}")))))%>% 
  unnest(data) %>% 
  unite(sample, criteria,i, sep = "-") %>% 
  pull(sample)

retained_samples <- tibble(file = list.files(here(new_dir), full.names = F)) %>% 
  filter(!grepl("20240525", file)) %>% 
  mutate(data = map(file, ~fromJSON(here(str_glue("{new_dir}/{.}")))))%>% 
  unnest(data)%>% 
  unite(sample, criteria,i, sep = "-") %>% 
  pull(sample)

original_samples <- tibble(file = list.files(here(original_dir), full.names = F)) %>% 
  mutate(data = map(file, ~fromJSON(here(str_glue("{original_dir}/{.}")))))%>% 
  unnest(data)%>% 
  unite(sample, criteria,i, sep = "-") %>% 
  pull(sample)

removed_samples <- setdiff(original_samples, retained_samples)
remaining_samples <- setdiff(removed_samples, new_samples)

print(str_glue("Number of original samples: {length(original_samples)}"))
print(str_glue("Number of samples removed: {length(removed_samples)}"))
print(str_glue("Number of samples retained: {length(retained_samples)}"))

print(str_glue("Number of new samples processed: {length(new_samples)}"))
print(str_glue("Number of new samples in retained samples: {length(intersect(new_samples, retained_samples))}"))
print(str_glue("Number of new samples in original samples: {length(intersect(new_samples, original_samples))}"))
print(str_glue("Number of new samples in removed samples: {length(intersect(new_samples, removed_samples))}"))

print(str_glue("Number of samples remaining to be processed: {length(remaining_samples)}"))