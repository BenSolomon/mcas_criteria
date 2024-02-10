require(tidyverse)
require(RColorBrewer)
require(broom)
require(igraph)
require(tidygraph)
require(ggraph)
require(ggplotify)
require(viridis)
require(vegan)

# Line graph of rank abundance by criteria. Will plot up to n_diagnoses number of top diagnoses
rank_abundance_plot <- function(df, n_diagnoses = 50){
  custom_pal <- brewer.pal(7, "Set1")[-6]
  
  df %>% 
    count(criteria, diagnosis, sort = T) %>% 
    group_by(criteria) %>% 
    mutate(freq = n/sum(n)) %>% 
    arrange(desc(freq), .by_group = T) %>% 
    mutate(rank = 1:n()) %>% 
    select(criteria, freq, rank) %>% 
    mutate(cs = cumsum(freq)) %>% 
    filter(rank <= n_diagnoses) %>% 
    mutate(criteria = toupper(criteria)) %>% 
    ggplot(aes(x = rank, y = freq, color = factor(criteria)))+
    geom_line(size=1) +
    theme_classic()+
    scale_color_manual(values = custom_pal)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Diagnosis rank", y = "Diagnosis frequency", color = "")
}

################################################################################

# Bar chart of top diagnoses for each criteria. Will plot up to n_diagnoses number of top diagnoses
# TODO order and format facet bars
top_diagnosis_plot <- function(df, n_diagnoses=25){
  df %>% 
    count(criteria, diagnosis, sort = T) %>% 
    mutate(freq = n/sum(n), .by = criteria) %>% 
    slice_max(n=n_diagnoses, order_by = n, by = criteria) %>% 
    mutate(group = tidytext::reorder_within(diagnosis, freq, within = criteria)) %>% 
    ggplot(aes(x = group, y = freq))+
    geom_bar(stat = "identity")+
    tidytext::scale_x_reordered()+
    theme_bw() +
    coord_flip() +
    labs(x="", y="Frequency") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~criteria, scales = "free_y")
}

################################################################################

# Bar chart of cumulative frequency for top n_diagnoses
# Also prints exact values of frequency sum
cumulative_frequency_plot <- function(df, n_diagnoses = 25){
  df <- df %>% 
    count(criteria, diagnosis, sort = T) %>% 
    mutate(freq = n/sum(n), .by = criteria) %>% 
    slice_max(n=n_diagnoses, order_by = n, by = criteria, with_ties = F) %>%
    summarise(total_frequency = sum(freq), .by = criteria)
  print(df)
  
  ggplot(df, aes(x = criteria, y = total_frequency))+
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = sprintf("Combined frequency of top %s diagnoses",n_diagnoses), x="")
}

################################################################################

# Based on a regex pattern, return a table with the rank of every match within 
# all criteria
disease_rank_table <- function(df, pattern){
  df %>% 
    count(criteria, diagnosis, sort = T) %>%
    mutate(rank = 1:n(), .by = criteria) %>% 
    mutate(freq = n/sum(n), .by = criteria) %>% 
    select(criteria, diagnosis, rank) %>% 
    filter(grepl(pattern, diagnosis)) %>% 
    pivot_wider(names_from = "criteria", values_from = "rank")
}

################################################################################

# Shannon diversity plot. Also prints values
diversity_plot <- function(df){
  diagnosis_table <- table(df$criteria, df$diagnosis)
  div_diag  <- vegan::diversity(diagnosis_table)
  print(div_diag)
  
  broom::tidy(div_diag) %>% 
    mutate(names = toupper(names)) %>% 
    ggplot(aes(x=names, y = x))+
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "", y = "Shannon diversity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


########################### GRAPH FUNCTIONS ####################################

# Recolors a faceted ggraph plot to make node colors specific to the facet, rather than globasl
ggraph_color_faceted_nodes <- function(plt, data, label_col, facet_col, color_col, na_color = "black"){
  # Adding a label text will create a data table with labeled rows that can be 
  # used for joining operations
  plt <- plt + geom_node_text(aes(label = name))
  # Create ggplot build object
  plt_build <- ggplot_build(plt)
  
  # Identify layers of interest
  plt_layers <- sapply(plt_build$plot$layers, function(x) class(x$geom)[[1]])
  plt_text_index <- which(plt_layers == "GeomText")
  plt_point_index <- which(plt_layers == "GeomPoint")
  
  # Information as to which criteria maps to which panel
  facet_key <- plt_build$layout$layout 
  
  # Using geom_text layer
  colors <- plt_build$data[[plt_text_index]] %>% 
    left_join(facet_key, by = "PANEL") %>% # Join facet criteria name
    left_join(data, by = c("label" = label_col, facet_col)) %>% # Join diagnosis info including colors
    mutate(color = replace_na(color, na_color)) %>% # Make all nodes not ranked in a criteria black (otherwise dropped)
    pull(color)
  
  # Assigning and reordering colors
  plt_build$data[[plt_point_index]]$fill <- colors # Assign colors to a color column. The order of rows in each layer data frame is the same
  plt_build$data[[plt_point_index]] <- plt_build$data[[plt_point_index]] %>% arrange(colour) # Bring any highlighted nodes to the top
  plt_build$data[[plt_text_index]] <- NULL # Remove geom_text layer
  
  # Build new plot
  plt_new <- ggplotify::as.ggplot(ggplot_gtable(plt_build))
  plt_new
}

################################################################################

# Maps a viridis color scale based on a vector of values x
map_color <- function(x, min, max, steps){
  df <- data.frame(
    value =  seq(from = min, to = max, length.out = steps)) %>% 
    mutate(color = viridis::viridis(n())) 
  color_index <- findInterval(x, df$value)
  df$color[color_index]
}

################################################################################

# Pipeline for generating a centrality-colored network plot with facets
centrality_graph <- function(graph, point_size=2.5){
  
  # Calculate centrality for the graph
  graph_ce <- graph %>%
    activate(nodes) %>% 
    mutate(ce = centrality_eigen())
  
  # Create a dataframe of the centrality values and assign colors based on map_color()
  df_ce <- data.frame(criteria = graph_ce %>% activate(edges) %>% pull(criteria) %>% unique()) %>% 
    mutate(sub_graphs = map(criteria, function(c){
      graph_ce %>% 
        activate(edges) %>% 
        filter(criteria == c) %>% 
        activate(nodes) %>% 
        mutate(ce = centrality_eigen()) %>% 
        data.frame()
    })) %>% 
    unnest(sub_graphs) %>% 
    mutate(color = map_color(ce, 0,1,100))
  
  # Specifically color the mastocytosis and MCAS nodes to highlight them
  graph_ce <- graph_ce %>%
    activate(nodes) %>%
    mutate(
      highlight = case_when(
        name == "mastocytosis" ~ "red",
        name == "mast cell activation syndrome" ~ "orange",
        .default = "black"
      )
    ) %>% 
    arrange(ce)
  
  # Create a labeler to format the facet labels
  criteria_names <- c(
    "mcas_consortium" = "MCAS Consensus",
    "mcas_alternative" = "MCAS Alternative",
    "eular_acr_sle" = "SLE EULAR-ACR",
    "slicc_sle" = "SLE SLICC")
  mcas_labeller <- as_labeller(criteria_names)
  
  # Generate the base plot. Node colors will be identical across all facets at this point
  plt <- ggraph(graph_ce, layout = 'fr', ) +
    geom_edge_link(alpha = 0.5) +
    geom_node_point(size = point_size, aes(color = highlight, fill = ce), shape = 21, stroke = 1) +
    scale_color_identity()+
    scale_fill_viridis_c(breaks = c(0.00,0.25,0.50,0.75,1.00), limits = c(0,1)) +
    facet_wrap(~criteria, labeller = mcas_labeller) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
    labs(color = "Centrality")

  # Recolor nodes so that color is specific to the facet of interest
  ggraph_color_faceted_nodes(plt,
                             df_ce,
                             label_col = "name",
                             facet_col = "criteria",
                             color_col = "color")
  
}