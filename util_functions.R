# Clean a diagnosis from a chatGPT output, removing list numbers, bullet points, 
# removes non-specific strings,
# removes white space, and converts to lower case to improve standardization
clean_diagnosis <- function(diagnosis){
  # Terms that modify many diagnosis that can be removed to consolidate variation
  patterns_to_remove <- c(
    "other unspecified",
    ", unspecified",
    "other specified",
    "with.*",
    ".*due to",
    ".*secondary to",
    " not .*",
    "^a "
    # "with normal pulmonary function tests",
    # "without abnormal pulmonary function tests",
    # "not otherwise specified",
    # "due to other specified causes",
    # "due to other specified bacterial agents",
    # "due to other specified non tuberculous bacterial agents",
    # "due to other specified infectious agents",
    # "with organ or system involvement",
    # "with other organ involvement",
    # "with organ involvement",
    # "without organ involvement",
    # "without organ or system involvement",
  )
  
  # Terms that are indicative of a "diagnosis" that should be removed
  # e.g. "it is crucial to consult with a healthcare professional"
  diagnoses_to_remove <- c(
    "language model",
    "healthcare professional",
    "medical attention",
    "medical professional"
  )
  
  diagnosis <- str_extract(diagnosis, "[a-zA-Z].*") # Remove all non-characters from beginning (e.g. "1)", " - ")
  diagnosis <- tolower(diagnosis) 
  diagnosis <- gsub("\\([a-z]*\\)","", diagnosis) # Remove lettered bullet points
  diagnosis <- gsub("\\.","", diagnosis)
  diagnosis <- gsub("\\-"," ", diagnosis) # ChatGPT inconsistently hyphenates
  diagnosis <- gsub("\\(.*\\)", "", diagnosis) # Remove any parenthetical like (e.g. x, y, z)
  diagnosis <- gsub(paste(patterns_to_remove, collapse = "|"),"",diagnosis)
  diagnosis <- gsub("\\,|\\/", " ", diagnosis) # Remove , \
  diagnosis <- iconv(diagnosis, to="ASCII//TRANSLIT") # Remove accents
  diagnosis <- trimws(diagnosis) 
  diagnosis <- gsub(" +", " ", diagnosis) # Convert 2 or more spaces in a row to one
  diagnosis <- diagnosis[!grepl(":", diagnosis)] # Remove lines that contain colons like "The top diagnoses are:"
  diagnosis <- diagnosis[!grepl(paste(diagnoses_to_remove, collapse = "|"), diagnosis)] # Remove diagnoses_to_remove
  diagnosis <- diagnosis[!is.na(diagnosis)] # Remove NAs
  diagnosis <- diagnosis[nchar(diagnosis) != 0] # Remove ""
  return(diagnosis)
}

# Takes the list of diagnoses from a single chatGPT query output,
# converts it into a vector, and applies the cleaning function
process_diagnoses <- function(resp){
  diagnoses <- str_split(resp, "\n") %>% 
    unlist() %>% 
    clean_diagnosis() 
  return(diagnoses)
}

# Calculate figure sizes
points_to_inches <- function(points=NA){
  if(is.na(points)){
    cat("Single column - 255 pts\n1.5 column - 397 pts\nTwo column - 539 pts")
  }
  else {
    points/72
  }
}



### ggraph can facet network graphs and can apply edge aesthetics according to
### facets, but can't apply node aesthetics according to facets (i.e. nodes
### will always be indentical across facets). This function takes a faceted
### ggraph plot and manually adjusts node colors according to a faceted variable
#
# data - datatable that will contain all node label : facet variable combinations 
#        and columns for node label, facet label, and color for each row
#
ggraph_color_faceted_nodes <- function(plt, data, label_col, facet_col, color_col, na_color = "black"){
  # Adding a label text will create a data table with labeled rows that can be 
  # used for joining operations
  plt <- plt + geom_node_text(aes(label = name))
  
  plt_build <- ggplot_build(plt)
  plt_layers <- sapply(plt_build$plot$layers, function(x) class(x$geom)[[1]])
  plt_text_index <- which(plt_layers == "GeomText")
  
  facet_key <- plt_build$layout$layout # Information as to which criteria maps to which panel
  
  # Using geom_text layer
  colors <- plt_build$data[[plt_text_index]] %>% 
    left_join(facet_key, by = "PANEL") %>% # Join facet criteria name
    left_join(data, by = c("label" = label_col, facet_col)) %>% # Join diagnosis info including colors
    mutate(color = replace_na(color, na_color)) %>% # Make all nodes not ranked in a criteria black (otherwise dropped)
    pull(color)
  
  plt_build$data[[plt_text_index-1]]$colour <- colors # Assign colors to a color column. The order of rows in each layer data frame is the same
  plt_build$data[[plt_text_index]] <- NULL # Remove geom_text layer
  
  plt_new <- ggplotify::as.ggplot(ggplot_gtable(plt_build))
  plt_new
}

### While facet labels can be adjusted during creation of a ggplot object,
### there is no function that allows labels to be adjusted after a plot is 
### stored as an object. This function allows facet labels to be adjusted after
### a plot is already created
#
# rename_key - a vector with entries "original label" = "new label"
#
ggplot_rename_facets <- function(plt, rename_key){
  plt_build <- ggplot_build(plt)
  facet_key <- plt_build$layout$layout
  label_col_index <- which(!(names(facet_key) %in% c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y")))
  facet_key[[label_col_index]] <- rename_key[facet_key[[label_col_index]]]
  plt_build$layout$layout <- facet_key
  ggplotify::as.ggplot(ggplot_gtable(plt_build))
}


