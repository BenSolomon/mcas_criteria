library(tidyverse)
library(iNEXT)
library(here)
source(here("utils/data_processing.R"))

model <- "gpt4"

read_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", model)
df <- read_csv(here(read_path))

df_cs <- df %>% 
  count(criteria, diagnosis) %>% 
  filter(n > 1) %>%
  mutate(n = n-1) %>% 
  pivot_wider(names_from = "criteria", values_from = n, values_fill = 0) %>% 
  column_to_rownames("diagnosis")

e <- 200000
iNEXT_out <- iNEXT(df_cs, q=c(0, 1, 2), datatype="abundance", endpoint = e)

saveRDS(iNEXT_out, here(sprintf("data/mcas_iNEXT_dropSingle_psuedoMinus_%s_e%s.RDS", model, format(e, scientific = F))))

