require(tidyverse)
require(RColorBrewer)
require(broom)
require(igraph)
require(tidygraph)
require(ggraph)
require(ggplotify)
require(viridis)
require(vegan)
require(flextable)
require(checkmate)
require(here)

source(here("utils/general.R"))

################################################################################
# Order levels for criteria for use in plots
format_criteria <- function(df){
  
  # Input checks, must be data frame with criteria column
  arg_col <- makeAssertCollection()
  assertClass(df, "data.frame" , add = arg_col)
  assertNames(names(df), must.include = "criteria", add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  criteria_order <- c(
    "migraine", 
    "aha_kawasaki", 
    "eular_acr_sle", 
    "slicc_sle",
    "mcas_consortium", 
    "mcas_alternative" 
    )
  
  criteria_names <- c(
    "mcas_consortium" = "MCAS - Consortium",
    "mcas_alternative" = "MCAS - Alternative",
    "eular_acr_sle" = "SLE - EULAR-ACR",
    "slicc_sle" = "SLE - SLICC",
    "aha_kawasaki" = "Kawasaki - AHA",
    "migraine" = "Migraine - ICHD3")
  
  df %>% 
    mutate(criteria = factor(criteria, levels = criteria_order)) %>%
    mutate(criteria = fct_recode(criteria, !!!setNames(names(criteria_names), criteria_names)))
}

################################################################################
# Order levels for models for use in plots
format_models <- function(df){
  
  # Input checks, must be data frame with model column
  arg_col <- makeAssertCollection()
  assertClass(df, "data.frame" , add = arg_col)
  assertNames(names(df), must.include = "model", add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  model_order <- c(
    "df_gpt3.5", 
    "df_gpt4.0", 
    "df_claude3_haiku_t1.0", 
    "df_claude3_opus_t1.0",
    "df_gemini1.0_pro_t1.0", 
    "df_gemini1.5_flash_t1.0" 
  )
  
  model_names <- c(
    "df_gpt3.5" = "ChatGPT 3.5",
    "df_gpt4.0" = "ChatGPT 4.0",
    "df_claude3_haiku_t1.0" = "Claude 3 Haiku",
    "df_claude3_opus_t1.0" = "Claude 3 Opus",
    "df_gemini1.0_pro_t1.0" = "Gemini 1.0 Pro",
    "df_gemini1.5_flash_t1.0" = "Gemini 1.5 Flash")
  
  df %>% 
    mutate(model = factor(model, levels = model_order)) %>%
    mutate(model = fct_recode(model, !!!setNames(names(model_names), model_names)))
}

################################################################################
# Function that wrap text in a ggplot function
# E.g. scale_x_discrete(labels = ~custom_labeler(., wrap_width = 20))
# Will wrap labels every 20 characters
custom_labeler <- function(x, wrap_width=33) {
  x %>%
    str_replace("___.+$", "") %>%
    str_wrap(width = wrap_width)
}

################################################################################
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
    format_criteria() %>% 
    ggplot(aes(x = rank, y = freq, color = factor(criteria)))+
    geom_line(linewidth=1) +
    theme_classic()+
    scale_color_manual(values = custom_pal)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Diagnosis rank", y = "Diagnosis frequency", color = "")
}

################################################################################
# Extended version of rank_abundance_plot that takes multiple diagnosis data
# frames from different models and plots the mean ranked frequencies with
# error bars
multi_ranked_abundance_plot <- function(...){
  df_list <- list(...)
  
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  lapply(df_list, function(x) assertClass(x, "data.frame" , add = arg_col))
  # Check all inputs have the expected columns
  expected_columns <- c("i", "criteria", "diagnosis")
  lapply(df_list, function(x) assertNames(names(x), permutation.of = expected_columns, add = arg_col))
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  # Combine data frames
  df_combined <- df_list %>% 
    lapply(., function(x) rank_abundance_plot(x)$data) %>% 
    bind_rows()
  
  # Plot
  df_combined %>% 
    ggplot(aes(x = rank, y = freq, color = criteria))+
    stat_summary(fun.data = mean_se, geom = "ribbon", fill = "grey75", color = "grey75", mapping = aes(group = criteria)) +
    stat_summary(fun.y = mean, geom = "path", size = 1) +
    scale_color_manual(values = brewer.pal(7, "Set1")[-6]) +
    theme_classic() +
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
    mutate(diagnosis = str_to_sentence(diagnosis)) %>% 
    slice_max(n=n_diagnoses, order_by = n, by = criteria) %>% 
    mutate(group = tidytext::reorder_within(diagnosis, freq, within = criteria)) %>% 
    format_criteria() %>% 
    ggplot(aes(x = group, y = freq))+
    geom_bar(stat = "identity", width = 0.8)+
    tidytext::scale_x_reordered()+
    # tidytext::scale_x_reordered(labels = custom_labeler)+
    theme_bw() +
    coord_flip() +
    labs(x="", y="Frequency") +
    theme(axis.text.x = element_text(angle = 90)) +
    ylim(c(0,0.1))+ # Since 10-item differential, max frequency is 0.1
    facet_wrap(~criteria, scales = "free_y", dir = "v", nrow = 2)
}

################################################################################
# Extended version of top_diagnosis_plot that takes multiple diagnosis data
# frames from different models and plots the ranked frequencies in order
# Several visualization options including different error bar approaches, 
# plotting individual points, etc.

multi_top_diagnosis_plot <- function(distribution_vis = "range", wrap_width=45, n_diag = 25, ...){
  df_list <- listN(...)
  
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  lapply(df_list, function(x) assertClass(x, "data.frame" , add = arg_col))
  # Check all inputs have the expected columns
  expected_columns <- c("i", "criteria", "diagnosis")
  lapply(df_list, function(x) assertNames(names(x), permutation.of = expected_columns, add = arg_col))
  # Check valid distribution_vis option
  assertChoice(distribution_vis, c("range", "std_error", "points") , add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  # Combine data into single data frame
  df_combined <- df_list %>% 
    mapply(function(x,y) {mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>% 
    bind_rows() %>% 
    count(model, criteria, diagnosis, sort = T) %>% 
    mutate(freq = n/sum(n), .by = c("model", "criteria"))
  
  # Create a key to reorder diagnoses in each criteria based on mean frequency  
  df_rank_key <- df_combined %>% 
    summarise(freq = mean(freq), .by = c("criteria", "diagnosis")) %>% 
    arrange(criteria, desc(freq)) %>% 
    mutate(rank = 1:n(), .by = "criteria") %>% 
    select(-freq)
  
  # Create base plot 
  base_plt <- df_combined %>% 
    left_join(df_rank_key, by = c("criteria", "diagnosis")) %>% 
    filter(rank <= n_diag) %>% 
    mutate(group = tidytext::reorder_within(diagnosis, freq, within = criteria)) %>% 
    format_criteria() %>% 
    format_models() %>% 
    ggplot(aes(x = group, y = freq))+
    theme_bw() +
    coord_flip() +
    labs(x="", y="Frequency")+
    theme(axis.text.x = element_text(angle = 90)) +
    ylim(c(0,0.1))+ # Since 10-item differential, max frequency is 0.1
    facet_wrap(~criteria, scales = "free_y", dir = "v", nrow = 2) +
    theme(axis.text = element_text(size = 7, lineheight = 0.7), 
          strip.text = element_text(size = 7),
          axis.title = element_text(size = 9)) + 
    tidytext::scale_x_reordered(labels = ~custom_labeler(., wrap_width = wrap_width))
  
  # Plot mean frequency as point and min-max as error bar  
  if (distribution_vis == "range"){
    out_plt <- base_plt +
      stat_summary(fun.y = mean, geom = "point") +
      stat_summary(fun.min = min, fun.max = max, geom = "errorbar")
  }  
  
  # Plot mean frequency as black point and individual model 
  # frequencies as colored points
  if (distribution_vis == "points"){
    out_plt <- base_plt +
      geom_point(size = 1, aes(color = model))+
      stat_summary(fun.y = mean, geom = "point") +
      scale_color_manual(values = brewer.pal(7, "Set1")[-6])+
      labs(color = "")
  }
  
  return(out_plt)
}
################################################################################
# Bar chart of cumulative frequency for top n_diagnoses
# Also prints exact values of frequency sum
cumulative_frequency_plot <- function(df, n_diagnoses = 25, width = 0.9){
  df <- df %>% 
    count(criteria, diagnosis, sort = T) %>% 
    mutate(freq = n/sum(n), .by = criteria) %>% 
    slice_max(n=n_diagnoses, order_by = n, by = criteria, with_ties = F) %>%
    summarise(total_frequency = sum(freq), .by = criteria) %>% 
    format_criteria() 
  
  plt <- ggplot(df, aes(x = criteria, y = total_frequency))+
    geom_bar(stat = "identity", color = "black", width = width) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = sprintf("Combined frequency of top\n%s diagnoses",n_diagnoses), x="")
  
  return(list("data" = df, "plot" = plt))
}

################################################################################
# Extended version of cumulative_frequency_plot that takes multiple diagnosis data
# frames from different models and plots the mean cumulative frequencies with
# error bars and statistical testing

multi_cumulative_frequency_plot <- function(...){
  df_list <- list(...)
  
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  lapply(df_list, function(x) assertClass(x, "data.frame" , add = arg_col))
  # Check all inputs have the expected columns
  expected_columns <- c("i", "criteria", "diagnosis")
  lapply(df_list, function(x) assertNames(names(x), permutation.of = expected_columns, add = arg_col))
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  # Combine data frames
  df_combined <- df_list %>% 
    lapply(., function(x) cumulative_frequency_plot(x)$plot$data) %>% 
    bind_rows()
  
  # Plot
  plt <- df_combined %>% 
    ggplot(aes(x = criteria, y = total_frequency))+
    stat_summary(fun.y = mean, geom = "point", size = 0.75)+
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = sprintf("Combined frequency of top\n%s diagnoses",n_diag), x="")
  
  # Statistical testing
  plt +
    ggpubr::geom_pwc(aes(group = criteria), 
                     method = "wilcox.test",
                     label = "p.signif",
                     p.adjust.method = "BH",
                     hide.ns = T,
                     vjust = 0.5)+
    ylim(c(0,NA))
}
################################################################################

# Based on a regex pattern, return a table with the rank of every match within 
# all criteria
diagnosis_rank_table <- function(df, pattern){
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
    rename(criteria = names) %>% 
    format_criteria() %>% 
    ggplot(aes(x=names, y = x))+
    geom_bar(stat = "identity", color = "black") +
    theme_bw() +
    labs(x = "", y = "Shannon diversity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

################################################################################

# Calculate bray-curtis or jaccard similarity of diagnoses counts
# between criteria and plot similarity as a heatmap
diagnosis_similarity_heatmap <- function(df, method = "jaccard"){
  df <- format_criteria(df)
  diagnosis_table <- table(df$criteria, df$diagnosis)
  dist_mtx <- vegan::vegdist(diagnosis_table, method = method)
  sim_mtx <- 1-as.matrix(dist_mtx)
  plt_title <- sprintf("%s\nsimilarity", str_to_sentence(method))
  ComplexHeatmap::Heatmap(sim_mtx,
                          name = plt_title,
                          col = viridis::viridis(100))
}

################################################################################

# Calculate PCA loadings for each criteria based on diagnosis count
# and plot criteria based on first two PCA loadings. 
diagnosis_pca_plot <- function(df){
  df <- format_criteria(df)
  diagnosis_table <- table(df$criteria, df$diagnosis)
  diagnosis_pca <- as.data.frame(diagnosis_table) %>% 
    pivot_wider(names_from = "Var2", values_from = "Freq", values_fill = 0) %>% 
    column_to_rownames("Var1") %>% 
    prcomp()
  
  as.data.frame(diagnosis_pca$x) %>% 
    rownames_to_column("criteria") %>% 
    ggplot(aes(x = PC1, y = PC2, label = criteria))+
    geom_point()+
    ggrepel::geom_label_repel() +
    theme_bw()
}

################################################################################
# Plot a visualization of how observed diversity or precision value differences
# between criteria compare to the null hypothesis permutation distribution of 
# those differences. Based on output from `difference_permutation_test`
permutation_test_plot <- function(df){
  permutations <- nrow(df$data[[1]])
  n_bins = ifelse(permutations > 100, floor(permutations/10), 10)
  
  df %>% 
    select(pair, data) %>% 
    unnest(data) %>% 
    ggplot(aes(x = abs(difference)))+
    geom_histogram(bins = n_bins) +
    geom_vline(data = df, aes(xintercept = difference), color = "red")+
    facet_wrap(~pair, scales = "free")+
    theme_bw() +
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
centrality_graph <- function(graph, 
                             layout = "fr",
                             point_size=2.5, 
                             border_size = 1,
                             edge_width = 1,
                             edge_alpha = 0.5,
                             label_text_size = NULL,
                             tick_text_size = NULL,
                             legend_width = NULL,
                             legend_height = NULL,
                             legend_position = NULL,
                             highlight_stroke_multiplier=2){
  # Calculate centrality for the graph
  graph_ce <- graph %>%
    mutate(criteria = factor(criteria, levels = c(
      "eular_acr_sle",
      "slicc_sle",
      "mcas_consortium",
      "mcas_alternative"
    ))) %>% 
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
  highlight_border_size = border_size*highlight_stroke_multiplier
  graph_ce <- graph_ce %>%
    activate(nodes) %>%
    mutate(
      highlight_color = case_when(
        name == "mastocytosis" ~ "red",
        name == "D47.02 Systemic mastocytosis" ~ "red",
        name == "mast cell activation syndrome" ~ "orange",
        name == "D89.41 Monoclonal mast cell activation syndrome" ~ "orange",
        .default = "black"
      ),
      highlight_stroke = case_when(
        name == "mastocytosis" ~ highlight_border_size,
        name == "D47.02 Systemic mastocytosis" ~ highlight_border_size,
        name == "mast cell activation syndrome" ~ highlight_border_size,
        name == "D89.41 Monoclonal mast cell activation syndrome" ~ highlight_border_size,
        .default = border_size
      )
    ) %>% 
    arrange(ce)
  
  # Create a labeler to format the facet labels
  criteria_names <- c(
    "eular_acr_sle" = "SLE EULAR-ACR",
    "slicc_sle" = "SLE SLICC",
    "mcas_consortium" = "MCAS Consortium",
    "mcas_alternative" = "MCAS Alternative"
    )
  mcas_labeller <- as_labeller(criteria_names)
  
  # Generate the base plot. Node colors will be identical across all facets at this point
  plt <- ggraph(graph_ce, layout = layout, ) +
    geom_edge_link(alpha = edge_alpha, edge_width = edge_width) +
    geom_node_point(size = point_size, aes(color = highlight_color, fill = ce, stroke = highlight_stroke), shape = 21) +
    scale_color_identity()+
    # scale_stroke_identity()+
    scale_fill_viridis_c(breaks = c(0.00,0.25,0.50,0.75,1.00), limits = c(0,1)) +
    facet_wrap(~criteria, labeller = mcas_labeller) +
    theme_void() +
    labs(fill = "Centrality") +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      strip.text = element_text(size = label_text_size),
      legend.title = element_text(size = label_text_size),
      legend.text = element_text(size = tick_text_size)
      ) +
    theme(legend.position = legend_position)
  
  if (!is.null(legend_width)){plt <- plt+theme(legend.key.width = legend_width)}
  if (!is.null(legend_height)){plt <- plt+theme(legend.key.height = legend_height)}

  # Recolor nodes so that color is specific to the facet of interest
  ggraph_color_faceted_nodes(plt,
                             df_ce,
                             label_col = "name",
                             facet_col = "criteria",
                             color_col = "color")
  
}
