library(here)
library(tidyverse)
library(xml2)

icd_xml <- read_xml(here("data/icd10cm-tabular-April-2024.xml"))

icd_data <- tibble(
  code=xml_find_all(icd_xml, "//diag/name") %>% xml_text(),
  diagnosis=xml_find_all(icd_xml, "//diag/desc") %>% xml_text()
)

write_csv(icd_data, here("data/compiled_icd10_codes.csv"))