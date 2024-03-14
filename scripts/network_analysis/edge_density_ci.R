# Calculate network edge density confidence intervals using snowboot::vertboot

library(here)
source(here("utils/data_processing.R"))
source(here("utils/figures.R"))

# Function to bootstrap a graph, calculate edge_density, and its 95% CI
boot_graph_ci <- function(g, B, seed=NULL){
  if (!is.null(seed)){set.seed(seed)}
  g <- as_adjacency_matrix(g)  
  g <- as.matrix(g) 
  g <- snowboot::vertboot(g, boot_rep = B)
  g <- sapply(1:B, function(x) edge_density(graph_from_adjacency_matrix(g[[x]])))
  quantile(g, c(0.025, 0.5, 0.975))
}

# Calculate edge density 95% CI for each criteria
edge_density_boot <- function(df, replications = 100, seed = 1234){
  df_density <- df %>%
    ungroup() %>%
    select(criteria, from, to, weight = n, rank) %>%
    group_by(criteria) %>%
    nest() %>%
    mutate(boot = map(data, function(g){
      g <-  graph_from_data_frame(g, directed = F)
      out <- boot_graph_ci(g, B=replications, seed=seed)
      out["edge_density"] = edge_density(g)
      out
    })) %>%
    select(-data) %>%
    unnest_wider(boot)
  return(df_density)
}



# model <- "gpt3"
model <- "gpt4"
# model <- "gpt4_icd"

data_path <- sprintf("data/processed_diagnoses/diagnoses_%s.csv.gz", model)

print("READING DATA")
df <- read_csv(here(data_path))
print("CREATING CO-DIAGNOSIS NETWORK")
df <- create_codiagnosis_df(df)
print("BOOTSTRAPPING NETWORKS")
df <- edge_density_boot(df, replications = 1000)
print("WRITING DATA")
write_path <- sprintf("data/edge_density_boot_%s.csv", model)
write_csv(df, here(write_path))

