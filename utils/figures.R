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
require(ComplexHeatmap)
require(cowplot)
require(grid)
require(here)

source(here("utils/general.R"))
####################### GENERAL FUNCTION #######################################
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
# Order levels for generaltive language models for use in plots
format_models <- function(df){
  
  # Input checks, must be data frame with model column
  arg_col <- makeAssertCollection()
  assertClass(df, "data.frame" , add = arg_col)
  assertNames(names(df), must.include = "model", add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  model_names <- c(
    "gpt3.5" = "ChatGPT 3.5",
    "gpt-3.5" = "ChatGPT 3.5",
    "gpt4.0" = "ChatGPT 4.0",
    "gpt-4" = "ChatGPT 4.0",
    "claude3_haiku" = "Claude 3 Haiku",
    "claude-3-haiku" = "Claude 3 Haiku",
    "claude3_opus" = "Claude 3 Opus",
    "claude-3-opus" = "Claude 3 Opus",
    "gemini1.0_pro" = "Gemini 1.0 Pro",
    "gemini-1.0-pro" = "Gemini 1.0 Pro",
    "gemini1.5_flash" = "Gemini 1.5 Flash",
    "gemini-1.5-flash" = "Gemini 1.5 Flash",
    "gemini1.5_pro" = "Gemini 1.5 Pro",
    "gemini-1.5-pro" = "Gemini 1.5 Pro")
  
  df %>% 
    mutate(df, model = str_extract(model, paste(names(model_names), collapse = "|"))) %>% 
    mutate(model = factor(model, levels = names(model_names))) %>%
    mutate(model = fct_recode(model, !!!setNames(names(model_names), model_names))) %>% 
    arrange(model)
}

################################################################################
# Order levels for embedding models for use in plots
format_embeddings <- function(df){
  
  # Input checks, must be data frame with model column
  arg_col <- makeAssertCollection()
  assertClass(df, "data.frame" , add = arg_col)
  assertNames(names(df), must.include = "model", add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  models <- c("chatgpt", "gemini", "voyage", "mistral")
  
  df <- mutate(df, model = str_extract(model, paste(models, collapse = "|")))
  
  model_order <- c(
    "chatgpt", 
    "voyage", 
    "gemini", 
    "mistral"
  )
  
  model_names <- c(
    "chatgpt" = "OpenAI Text 3 Small",
    "voyage" = "Voyage 2 Large",
    "gemini" = "Gemini Text 4",
    "mistral" = "Mistral Embed")
  
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
# Function that adds indicated layer to a base ggplot. Meant to be used with 
# multi plots, defining how to visualize the multiple models
# "range" - mean point + max/min error bar
# "std_error" - mean point + stderr error bars
# "points" - mean bar and individual model points

plot_selector <- function(distribution_vis = "range"){
  arg_col <- makeAssertCollection()
  assertChoice(distribution_vis, c("range", "std_error", "points") , add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  # Plot mean frequency as point and min-max as error bar  
  if (distribution_vis == "range"){
    out_plt <- list(
      stat_summary(fun.y = mean, geom = "point"),
      stat_summary(fun.min = min, fun.max = max, geom = "errorbar")
    )
  }  
  
  # Plot mean frequency as point and min-max as error bar  
  if (distribution_vis == "std_error"){
    out_plt <- list(
      stat_summary(fun.y = mean, geom = "point", size = 0.75),
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)
    )
  }  
  
  # Plot mean frequency as black point and individual model 
  # frequencies as colored points
  if (distribution_vis == "points"){
    out_plt <- list(
      # geom_point(size = 0.75, aes(color = model), position = position_dodge(width = 0.25)),
      ggbeeswarm::geom_beeswarm(aes(color = model), size = 0.75, cex=2),
      stat_summary(fun.y = mean, geom = "crossbar", size = 0.25, width = 0.75),
      scale_color_brewer(palette = "Dark2"),
      labs(color = NULL)
    )
  }
  return(out_plt)
}

################################################################################
# When a ggplot includes a ggpubr::geom_pwc layer, this function will extract
# the p-values from the plot

extract_ggpubr_pvalues <- function(plt){
  plt <- ggplot_build(plt)
  plt_data <- plt$data
  ggpubr_layer_index <- which(sapply(plt_data, function(x) "p" %in% names(x)))
  ggpubr_data <- plt_data[[ggpubr_layer_index]]
  
  key <- data.frame(criteria = plt$layout$panel_params[[1]]$x$get_labels()) %>% 
    mutate(index = as.character(1:n()))
  
  ggpubr_data %>% 
    left_join(key, by = c("group1" = "index")) %>% 
    left_join(key, by = c("group2" = "index")) %>% 
    unite(comparison, starts_with("criteria"), sep = " - ") %>% 
    select(comparison, starts_with("p")) %>% 
    distinct()
}

################################################################################
# Print a table with Elseviers plot dimension guidelines
elsevier_fig_dims <- function(){
  tribble(
    ~"size", ~"mm", ~"pts", ~"300dpi_pixels", ~"500dpi_pixels", ~"1000dpi_pixels",
    "Minimal size",	30,	85,	354,	591,	1181,
    "Single column",	90,	255,	1063,	1772,	3543,
    "1.5 column",	140,	397,	1654,	2756,	5512,
    "Double column (full width)",	190,	539,	2244,	3740,	7480
  ) %>% 
    mutate(inch=round(mm*0.0393701, 1)) %>% 
    select(size, mm, inch, everything())
}


################################################################################
# Custom ggpubr p-value annotation that only shows comparisons that include MCAS

mcas_only_pwc <- function(data, variable, max_y = NULL) {
  if (is.null(max_y)) {
    range <- max(data[[variable]]) - min(data[[variable]])
    max_y <- max(data[[variable]]) + range
  }
  form <- as.formula(str_glue("{variable} ~ criteria"))
  stats <- data %>% 
    rstatix::wilcox_test(form, p.adjust.method = "BH") %>% 
    rstatix::add_significance(p.col = "p.adj") %>% 
    filter(p.adj < 0.05) %>%
    filter(grepl("MCAS", group2) | grepl("MCAS", group2)) %>% 
    mutate(y.position = seq(max(data[[variable]]), max_y, length.out = n()))
  print(stats)
  
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", bracket.nudge.y = 0.1, vjust = 0.6, step.increase = 0, tip.length = 0.02)
}

#################### EMBEDDING FUNCTIONS #######################################
################################################################################
# Plots cumulative proportion of variance explained by each component in PCA
# Used to visualize variability captured in PCA reduction of embeddings
cumulative_variance_plot <- function(pca_data){
  pca_data$importance %>% t() %>% as.data.frame() %>% 
    rownames_to_column("component") %>% 
    mutate(component = as.numeric(str_extract(component, "[0-9]+"))) %>% 
    ggplot(aes(x=component, y = `Cumulative Proportion`)) +
    geom_path()+
    theme_bw()
}

################################################################################
# Plot embedding PCA reductions with diagnostic criteria centroids
pca_centroid_plot <- function(pca_data, criteria_key, components = c(1,2)){
  axes <- colnames(pca_data$x)[components]
  
  pca_data$x[,components] %>% 
    as.data.frame() %>% 
    rownames_to_column("feature") %>% 
    left_join(criteria_key) %>% 
    drop_na() %>% 
    ggplot(aes(x=!!sym(axes[1]), y=!!sym(axes[2]), color = criteria))+
    geom_point(size = 0.5) +
    geom_point( # Add centroids
      data = . %>% group_by(criteria) %>% summarise_at(vars(contains("PC")), mean), 
      size = 3, shape = 21, color = "black", aes(fill = criteria))+
    theme_bw()+
    scale_color_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2") +
    labs(color = "", fill = "")
}

################################################################################
# Extended version of pca_centroid_plot plots embedding PCAs for multiple
# embedding models as facets. Cannot combine facets into single plot
# because PCA reductions are different coordinate systems
multi_pca_centroid_plot <- function(criteria_key, components = c(1,2), ...){
  
  pca_list <- listN(...)
  
  axes <- colnames(pca_list[[1]]$x)[components]
  
  pca_list %>% 
    lapply(., function(x) {
      x <- x$x[,components]
      x <- as.data.frame(x)
      rownames_to_column(x, "feature")
    }) %>% 
    mapply(function(x,y){mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>% 
    bind_rows() %>% 
    left_join(criteria_key) %>% 
    drop_na() %>%
    format_embeddings() %>% 
    ggplot(aes(x=!!sym(axes[1]), y=!!sym(axes[2]), color = criteria))+
    geom_point(size = 0.5) +
    geom_point(data = . %>% group_by(criteria) %>% summarise_at(vars(contains("PC")), mean), size = 2, shape = 21, color = "black", aes(fill = criteria))+
    facet_wrap(~model, scales = "free") +
    theme_bw()+
    scale_color_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2") +
    labs(color = "", fill = "")
}


################### DIVERSITY FUNCTIONS ########################################
################################################################################
# Line graph of rank abundance by criteria. Will plot up to n_diagnoses number of top diagnoses
rank_abundance_plot <- function(df, n_diagnoses = 50){
  custom_pal <- brewer.pal(7, "Set1")[-6]
  
  df <- df %>% 
    count(criteria, diagnosis, sort = T) %>% 
    group_by(criteria) %>% 
    mutate(freq = n/sum(n)) %>% 
    arrange(desc(freq), .by_group = T) %>% 
    mutate(rank = 1:n()) %>% 
    select(criteria, freq, rank) %>% 
    mutate(cs = cumsum(freq))
  
  if (!is.null(n_diagnoses)){
    df <- filter(df, rank <= n_diagnoses)
  }
  
  df %>% 
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
multi_ranked_abundance_plot <- function(..., n_diagnoses = 50){
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
    lapply(., function(x) rank_abundance_plot(x, n_diagnoses=n_diagnoses)$data) %>% 
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

multi_top_diagnosis_plot <- function(..., distribution_vis = "range", wrap_width=45, n_diag = 25){
  df_list <- listN(...)
  
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  lapply(df_list, function(x) assertClass(x, "data.frame" , add = arg_col))
  # Check all inputs have the expected columns
  expected_columns <- c("i", "criteria", "diagnosis")
  lapply(df_list, function(x) assertNames(names(x), permutation.of = expected_columns, add = arg_col))
  # Check valid distribution_vis option
  assertChoice(distribution_vis, c("range", "std_error", "points", "data") , add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  # Combine data into single data frame
  df_combined <- df_list %>% 
    mapply(function(x,y) {mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>% 
    bind_rows() %>% 
    count(model, criteria, diagnosis, sort = T) %>% 
    complete(model, criteria, diagnosis, fill=list(n=0)) %>% 
    mutate(freq = n/sum(n), .by = c("model", "criteria")) 
  
  # Create a key to reorder diagnoses in each criteria based on mean frequency  
  df_rank_key <- df_combined %>% 
    summarise(freq = mean(freq), .by = c("criteria", "diagnosis")) %>% 
    arrange(criteria, desc(freq)) %>% 
    mutate(rank = 1:n(), .by = "criteria") %>% 
    select(-freq)
  
  if (distribution_vis == "data"){
    return(df_combined)
  }
  
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
    labs(x=NULL, y="Frequency")+
    theme(axis.text.x = element_text(angle = 90)) +
    ylim(c(0,0.1))+ # Since 10-item differential, max frequency is 0.1
    facet_wrap(~criteria, scales = "free_y", dir = "h", nrow = 3) +
    theme(axis.text = element_text(size = 7, lineheight = 0.7), 
          strip.text = element_text(size = 7),
          axis.title = element_text(size = 9)) + 
    tidytext::scale_x_reordered(labels = ~custom_labeler(., wrap_width = wrap_width))
  
  out_plt <- base_plt + plot_selector(distribution_vis)
  
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

multi_cumulative_frequency_plot <- function(n_diagnoses, distribution_vis = "points", ...){
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
  
  # Combine data frames
  df_combined <- df_list %>% 
    lapply(., function(x) cumulative_frequency_plot(x, n_diagnoses = n_diagnoses)$plot$data) %>% 
    mapply(function(x,y) {mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>% 
    bind_rows() %>% 
    format_models()
  
  # Plot
  base_plt <- df_combined %>% 
    ggplot(aes(x = criteria, y = total_frequency))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = str_glue("Combined frequency\nof top {n_diagnoses} diagnoses"), x = NULL)
  
  # Statistical testing
  base_plt <- base_plt +
    ggpubr::geom_pwc(method = "wilcox.test", p.adjust.method = "BH", hide.ns = T, label = "p.adj.signif", bracket.nudge.y = 0.3, vjust = 0.6, step.increase = 0.14, tip.length = 0.02)+
    ylim(c(0,NA))
  
  out_plt <- base_plt + plot_selector(distribution_vis)
  
  return(out_plt)
  
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
# Extension of diagnosis_rank_table that takes responses from multiple models
# and finds the ranks of specific diagnoses according to a search pattern. 
# Finds the average rank across all models and also returns the 
# individual model rankings for reference. Mean + ranks returned as a string
# intended for visualization with a flextable

multi_diagnosis_rank_table <- function(...,search_pattern){
  df_combined <- multi_top_diagnosis_plot(distribution_vis = "data", ...)
  
  df_out <- df_combined %>% 
    arrange(model, criteria, desc(n)) %>% 
    mutate(rank = min_rank(desc(n)), .by=c("model", "criteria")) %>% 
    mutate(rank = ifelse(n==0, NA, rank)) %>% 
    format_models() %>% 
    format_criteria() %>% 
    summarise(mean_freq = mean(freq), ranks = paste(rank, collapse = ", "), .by = c("criteria", "diagnosis")) %>% 
    mutate(mean_rank = min_rank(desc(mean_freq)), .by=c("criteria")) %>% 
    filter(grepl(search_pattern, diagnosis)) %>% 
    filter(grepl("MCAS", criteria)) %>% 
    mutate(output = str_glue("{mean_rank}\n[{ranks}]")) %>% 
    arrange(criteria, mean_rank) %>% 
    select(Diagnosis = diagnosis, criteria, output) %>%
    pivot_wider(names_from = "criteria", values_from = "output")
  
  return(df_out)
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
# Calculate shannon diversity for responses of multiple models and plot 
# differences between criteria. # Includes wilcox test of means and
# distribution_vis control visualization of diversity values for models

# Helper functionto calculate diversity
calculate_shannon <- function(df){
  table(df$criteria, df$diagnosis) %>% 
    vegan::diversity()
}

multi_shannon_plot <- function(..., distribution_vis = "range", wrap_width=45, n_diag = 25,
                               only_mcas_p=F){
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
    lapply(function(x) enframe(calculate_shannon(x), "criteria", "shannon")) %>%
    mapply(function(x,y) {mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>%
    bind_rows() %>% 
    format_criteria() %>% 
    format_models()
  
  # Create base plot 
  base_plt <- df_combined %>% 
    ggplot(aes(x = criteria, y = shannon))+
    theme_bw()+
    labs(x=NULL, y="Shannon diversity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # Statistical testing
  if (only_mcas_p==T){
    base_plt <- base_plt+
      mcas_only_pwc(df_combined, variable = "shannon")
  } else {
    base_plt <- base_plt +
      ggpubr::geom_pwc(
        method = "wilcox.test",
        p.adjust.method = "BH",
        hide.ns = T,
        label = "p.adj.signif",
        bracket.nudge.y = 0.3,
        vjust = 0.6,
        step.increase = 0.14,
        tip.length = 0.02
      )
  }
  
  out_plt <- base_plt + plot_selector(distribution_vis)
  
  return(out_plt)
}
################################################################################

# Calculate bray-curtis or jaccard similarity of diagnoses counts
# between criteria and plot similarity as a heatmap
diagnosis_similarity_heatmap <- function(df, method = "jaccard",
                                         label_size=6, title_size=9){
  df <- format_criteria(df)
  diagnosis_table <- table(df$criteria, df$diagnosis)
  dist_mtx <- vegan::vegdist(diagnosis_table, method = method)
  sim_mtx <- 1-as.matrix(dist_mtx)
  plt_title <- sprintf("%s\nsimilarity", str_to_sentence(method))
  ComplexHeatmap::Heatmap(sim_mtx,
                          name = plt_title,
                          col = viridis::viridis(100),
                          rect_gp = grid::gpar(col = "black", lwd = 1),
                          row_names_gp = grid::gpar(fontsize = label_size),
                          column_names_gp = grid::gpar(fontsize = label_size),
                          show_row_dend = FALSE,
                          show_column_dend = FALSE
                          )
}

################################################################################
# Extension of diagnosis_similarity_heatmap for multiple models
# Finds and plots average of similarity across models for each criteria

# Helper function to calculate jaccard or bray-curtis distance
calculate_distance <- function(df, method){
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  assertClass(df, "data.frame" , add = arg_col)
  # Check all inputs have the expected columns
  expected_columns <- c("i", "criteria", "diagnosis")
  assertNames(names(df), permutation.of = expected_columns, add = arg_col)
  # Check valid distribution_vis option
  assertChoice(method, c("jaccard", "bray") , add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  df <- format_criteria(df)
  diagnosis_table <- table(df$criteria, df$diagnosis)
  dist_mtx <- vegan::vegdist(diagnosis_table, method = method)
  dist_df <- as.dist(dist_mtx, diag = T) %>% broom::tidy()
  # sim_df <- mutate(dist_df, similarity = 1-distance)
  return(dist_df)
}


multi_diagnosis_similarity_heatmap <- function(..., method = "jaccard", show_dend=T,
                                               dendrogram_weight = unit(10, "mm"),
                                               legend_label = NULL,legend_direction="vertical",
                                               label_size=6, title_size=9){
  
  # browser()
  df_list <- listN(...)
  
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  lapply(df_list, function(x) assertClass(x, "data.frame" , add = arg_col))
  # Check all inputs have the expected columns
  expected_columns <- c("i", "criteria", "diagnosis")
  lapply(df_list, function(x) assertNames(names(x), permutation.of = expected_columns, add = arg_col))
  # Check valid distribution_vis option
  assertChoice(method, c("jaccard", "bray") , add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  df_combined <- df_list %>% 
    lapply(calculate_distance, method) %>%
    mapply(function(x,y) {mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>%
    bind_rows() %>% 
    # format_criteria() %>% 
    format_models() %>% 
    summarize(distance = mean(distance), .by = c("item1", "item2"))
  
  mtx_combined <- df_combined %>%
    pivot_wider(names_from = "item1", values_from = "distance") %>%
    column_to_rownames("item2") %>% 
    as.dist(diag = T, upper = T) %>% 
    as.matrix()
  
  sim_mtx <- 1-as.matrix(mtx_combined)
  if (is.null(legend_label)){legend_label <- sprintf("%s\nsimilarity", str_to_sentence(method))}
  
  legend_params <- list(
    direction = legend_direction, 
    title_gp  = grid::gpar(fontsize = title_size),
    labels_gp = grid::gpar(fontsize = label_size),
    legend_width = unit(3, "cm")
  )
  
  heatmap_params <- list(matrix = sim_mtx,
                          name = legend_label,
                          col = viridis::viridis(100),
                          rect_gp = grid::gpar(col = "black", lwd = 1),
                          row_names_gp = grid::gpar(fontsize = label_size),
                          column_names_gp = grid::gpar(fontsize = label_size),
                          show_row_dend = show_dend,
                          show_column_dend = show_dend,
                          heatmap_legend_param = legend_params
  )
  
  if(show_dend){
    heatmap_params[['column_dend_height']] <- dendrogram_weight
    heatmap_params[['row_dend_width']] <- dendrogram_weight
  }
  
  do.call(Heatmap, heatmap_params)
}

################################################################################
# Generalized heatmap for any distance matrix. Expects row and column names
# to include both the model and the criteria being compared.
# Will use the model and criteria name to create an annotation key

model_criteria_heatmap <- function(data,
                                      label_sep,
                                      title, 
                                      metric, 
                                      color_scale = hcl.colors(3, "Earth"), 
                                      midpoint = NULL, 
                                      symmetric = T, 
                                      font_size=10,
                                      dendrogram_weight = unit(10, "mm"),
                                      legend_params = list(NULL)){
  # browser()
  scale_max <- ifelse(symmetric,max(abs(data)),max(data))
  scale_min <- ifelse(symmetric,-max(abs(data)),min(data))
  scale_mid <- scale_min + (scale_max - scale_min)/2
  
  midpoint <- ifelse(is.null(midpoint), scale_mid, midpoint)
  
  color_function <-circlize::colorRamp2(c(scale_min,midpoint,scale_max), color_scale)
  
  annotation_data <- data.frame(
    rownames = rownames(data),
    model = str_extract(rownames(data), "gpt3.5|gpt4.0|claude3_haiku|claude3_opus|gemini1.0_pro|gemini1.5_pro"),
    criteria = str_extract(rownames(data), "migraine|aha_kawasaki|slicc_sle|eular_acr_sle|mcas_consortium|mcas_alternative")
  ) %>% 
    column_to_rownames("rownames") %>% 
    format_criteria() %>% 
    format_models()
  
  # model_colors <- brewer.pal(7, "Dark2")[-6]
  models <- unique(annotation_data$model)
  model_colors <- colorspace::lighten(brewer.pal(length(models), "Dark2"), amount = -0.1)
  names(model_colors) <- models
  
  criteria <- unique(annotation_data$criteria)
  criteria_colors <- colorspace::lighten(brewer.pal(length(criteria), "Set1"), amount = 0.3)
  names(criteria_colors) <- criteria
  
  col_annotation <- HeatmapAnnotation(
    Model = annotation_data$model,
    Criteria = annotation_data$criteria,
    col = list(Model = model_colors, Criteria = criteria_colors)
  )
  
  ComplexHeatmap::Heatmap(
    data,
    col = color_function,
    top_annotation = col_annotation,
    show_row_names = FALSE,
    show_column_names = FALSE,
    cluster_columns = TRUE,
    cluster_rows = TRUE,
    
    column_title = title,
    name = metric,
    column_dend_height  = dendrogram_weight,
    row_dend_width = dendrogram_weight,
  )
  
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
################################################################################
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
        name == "D89.49 Other mast cell activation disorder" ~ "orange",
        .default = "black"
      ),
      highlight_stroke = case_when(
        name == "mastocytosis" ~ highlight_border_size,
        name == "D47.02 Systemic mastocytosis" ~ highlight_border_size,
        name == "mast cell activation syndrome" ~ highlight_border_size,
        name == "D89.41 Monoclonal mast cell activation syndrome" ~ highlight_border_size,
        name == "D89.49 Other mast cell activation disorder" ~ highlight_border_size,
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

################################################################################
# Extended version of centrality_graph that takes multiple diagnosis data
# frames from different models. Two options as to how top_n co-diagnoses
# are determined: either selecting all co-diagnoses from any model meeting
# the threshold or taking the average frequency and selecting the top_n from
# the average frequency across all models
multi_make_codiagnosis_graph <-
  function(..., threshold_method = "individual",
           top_n = 200,
           layout = "stress",
           point_size=2.5, 
           border_size = 1,
           edge_width = 1,
           edge_alpha = 0.5,
           label_text_size = NULL,
           tick_text_size = NULL,
           legend_width = NULL,
           legend_height = NULL,
           legend_position = NULL,
           highlight_stroke_multiplier=2
           ) {
    
  df_list <- list(...)
  
  # Input checks
  arg_col <- makeAssertCollection()
  # Check all inputs are data frames
  lapply(df_list, function(x) assertClass(x, "data.frame" , add = arg_col))
  # Check all inputs have the expected columns
  expected_columns <- c("from", "to", "n", "criteria", "rank", "freq")
  lapply(df_list, function(x) assertNames(names(x), permutation.of = expected_columns, add = arg_col))
  # Check appropriate threshold_method
  assertChoice(threshold_method, choices = c("individual", "average"), add = arg_col)
  if (arg_col$isEmpty()==F) {map(arg_col$getMessages(),print);reportAssertions(arg_col)}
  
  df_combine <- bind_rows(df_list)
  
  # Selects all co-diagnoses within the top_n in any model
  if (threshold_method=="individual"){
    df_combine <- df_combine %>% 
      filter(rank <= top_n) %>% 
      select(criteria, from, to) %>% 
      mutate(rank = 0) %>% 
      distinct()
  }
  
  # Selects top_n co-diagnoses based on average frequency across all models
  if (threshold_method=="average"){
    df_combine <- df_combine %>% 
      summarise(freq = mean(freq), .by = c("criteria", "from", "to")) %>% 
      arrange(desc(freq)) %>% 
      mutate(rank = 1:n(), .by = c("criteria"))
  }
  
  df_combine %>% 
    make_codiagnosis_graph(n_diagnoses = top_n) %>% 
    centrality_graph(layout = layout,
                     point_size=point_size, 
                     border_size = border_size,
                     edge_width = edge_width,
                     edge_alpha = edge_alpha,
                     label_text_size = label_text_size,
                     tick_text_size = tick_text_size,
                     legend_width = legend_width,
                     legend_height = legend_height,
                     legend_position = legend_position,
                     highlight_stroke_multiplier=highlight_stroke_multiplier)
}

################################################################################
# Calculates the edge density for each graph for a given criteria and model
# Plots the mean and std error of these edge densities for each criteria
# across the difference models and applies statistical testing
multi_edge_density_plot <- function(...){
  df_list <- listN(...)
  df_list <- lapply(df_list, ungroup)
  
  # Process data
  df_combined <- df_list %>% 
    mapply(function(x,y) {mutate(x, model=y)}, ., names(.), SIMPLIFY = F) %>% 
    bind_rows() %>% 
    nest(.by = c("model", "criteria")) %>% 
    mutate(edge_density = map_dbl(data, ~edge_density(graph_from_data_frame(.)))) %>% 
    select(-data)
  
  # Create plot
  plt <- df_combined %>% 
    format_criteria() %>% 
    format_models() %>% 
    ggplot(aes(x = criteria, y=edge_density,))+
    # geom_point(aes( color = model), position = position_dodge(width = 0.75))+
    # stat_summary(fun.y = mean, geom = "point") + 
    # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x=NULL, y="Edge density")
  
  # Add statistics
  plt <- plt + 
    ggpubr::geom_pwc(aes(group = criteria), 
                     method = "wilcox.test",
                     label = "p.signif",
                     p.adjust.method = "BH",
                     hide.ns = T,
                     vjust = 0.5)+
    scale_color_manual(values = brewer.pal(7, "Dark2")[-6])+
    labs(color = "")
  
  out_plt <- plt + plot_selector(distribution_vis = "points")
  return(out_plt)
}
