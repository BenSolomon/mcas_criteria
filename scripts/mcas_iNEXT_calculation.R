library(tidyverse)
library(iNEXT)
library(here)

df <- read_csv(here("data/compiled_mcas_chatgpt_diagnoses.csv"))

df_cs <- df %>% 
  count(consensus, diagnosis) %>% 
  pivot_wider(names_from = "consensus", values_from = n, values_fill = 0) %>% 
  column_to_rownames("diagnosis")

# iNEXT_out <- iNEXT(ls_cs, q=c(0, 1, 2), datatype="abundance", endpoint=5)
e <- 500000
iNEXT_out <- iNEXT(df_cs, q=c(0, 1, 2), datatype="abundance", endpoint = e)

saveRDS(iNEXT_out, here(sprintf("data/mcas_iNEXT_e%s.RDS", format(e, scientific = F))))

