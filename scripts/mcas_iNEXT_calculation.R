library(tidyverse)
library(iNEXT)
library(here)

df <- read_csv(here("data/compiled_all_chatgpt_diagnoses.csv")) %>% 
  filter(grepl("mcas", criteria))

df_cs <- df %>% 
  count(criteria, diagnosis) %>% 
  pivot_wider(names_from = "criteria", values_from = n, values_fill = 0) %>% 
  column_to_rownames("diagnosis")

# iNEXT_out <- iNEXT(ls_cs, q=c(0, 1, 2), datatype="abundance", endpoint=5)
e <- 250000
iNEXT_out <- iNEXT(df_cs, q=c(0, 1, 2), datatype="abundance", endpoint = e)

saveRDS(iNEXT_out, here(sprintf("data/mcas_iNEXT_e%s.RDS", format(e, scientific = F))))

