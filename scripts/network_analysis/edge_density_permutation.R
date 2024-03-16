library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))
library(future.apply)

threads <- 48
n_boot <- 500
model <- "gpt4"

read_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", model)
df <- read_csv(here(read_path))
df_codiag <- create_codiagnosis_df(df)

df_density <- df_codiag %>%
  ungroup() %>%
  select(criteria, from, to, weight = n, rank) %>%
  group_by(criteria) %>%
  nest() %>%
  mutate(data = map_dbl(data, function(g){
    g <-  graph_from_data_frame(g, directed = F)
    edge_density(g)
  })) %>% 
  deframe()


difference_dist <- function(x){
  d <- dist(x, method = "manhattan")
  v <- as.dist(apply(combn(x, 2, simplify = T), 2, diff))
  d[1:length(v)] <- v
  broom::tidy(d)
}

criteria_levels <- c(
  "eular_acr_sle",
  "slicc_sle",
  "mcas_consortium",
  "mcas_alternative"
)

graph_permutation_diff <- function(){
  t(combn(c(
    "eular_acr_sle",
    "mcas_alternative",
    "mcas_consortium",
    "slicc_sle"),
    2)) %>% 
    as.data.frame() %>% 
    mutate(data = map2(V1, V2, function(x,y){
      filter(df, criteria == x | criteria == y) %>% 
        mutate(diagnosis = sample(diagnosis, replace = F)) %>% 
        create_codiagnosis_df()
    })) %>% 
    mutate(data = map_dbl(data, function(d){
      ungroup(d) %>%
        select(criteria, from, to, weight = n, rank) %>% 
        nest(.by = criteria) %>% 
        mutate(data = map_dbl(data, ~edge_density(graph_from_data_frame(.)))) %>% 
        deframe() %>% diff()
    })) %>% 
    unite(comparison, V1, V2, sep = "__") %>% 
    deframe()
}


plan(multisession, workers = threads)
permutation_data <- future_replicate(n = n_boot, graph_permutation_diff(), future.seed = 1234)

permutation_data <- as.data.frame(permutation_data) %>% 
  rownames_to_column("pair") %>% 
  separate(pair, into = c("item1", "item2"), sep = "__") %>% 
  mutate(data = map2(item1, item2, ~sort(factor(c(.x,.y), levels = criteria_levels)))) %>% 
  unnest_wider(data, names_sep = "") %>% 
  mutate(reverse = item1 != data1) %>% 
  unite(pair, data1, data2, sep = "__") %>% 
  select(!contains("item")) %>% 
  pivot_longer(!c(pair, reverse), names_to = "i", values_to = "difference") %>% 
  mutate(difference = ifelse(reverse, -difference, difference)) %>% 
  select(-reverse)

original_data <- difference_dist(df_density) %>% 
  rename(difference = distance) %>% 
  mutate_at(vars(c(item1, item2)), as.character) %>% 
  mutate(data = map2(item1, item2, ~sort(factor(c(.x,.y), levels = criteria_levels)))) %>% 
  unnest_wider(data, names_sep = "") %>% 
  mutate(reverse = item1 != data1) %>% 
  mutate(difference = ifelse(reverse, -difference, difference)) %>% 
  unite(pair, data1, data2, sep = "__") %>% 
  select(pair, difference)

permutation_plot_raw <- ggplot(permutation_data, aes(x = difference))+
  geom_histogram()+
  geom_vline(data = original_data, aes(xintercept = difference))+
  facet_wrap(~pair)+
  theme_bw()

permutation_plot_abs <- ggplot(permutation_data, aes(x = abs(difference)))+
  geom_histogram()+
  geom_vline(data = original_data, aes(xintercept = abs(difference)))+
  facet_wrap(~pair)+
  theme_bw()

permutation_significance_abs <- permutation_data %>% 
  summarise(permutation = list(difference), .by = pair) %>% 
  left_join(original_data) %>% 
  mutate(
    abs_quantile = map2_dbl(difference, permutation, ~1-ecdf(abs(.y))(abs(.x)))
  ) %>% 
  select(pair, abs_quantile)

output_list <- list(
  "original_data" = original_data,
  "permutation_data" = permutation_data,
  "permutation_significance_abs" = permutation_significance_abs,
  "permutation_plot_raw" = permutation_plot_raw,
  "permutation_plot_abs" = permutation_plot_abs
)

write_path <- sprintf("data/network_analysis/edge_density_permutation_%s.RDS", model)
saveRDS(output_list, here(write_path))