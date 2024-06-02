################################################################################
# Function that creates a list where every element is named
# based on the variable label of that element
# E.g. listN(iris) = list("iris" = iris)

listN <- function(...) {
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}

################################################################################
# Take multiple dataframes with the same columns and combine them into
# a single dataframe with an additional column 'original_df' indicating
# which original dataframe each entry originated from
# Ability to apply an additional function prior to row binding

combine_data_frames <- function(..., additional_function=NULL){
  dfs <- listN(...)
  if(!is.null(additional_function)){dfs <- lapply(dfs, additional_function)}
  dfs <- mapply(function(x,y) {x[['original_df']] <- y;x}, dfs, names(dfs), SIMPLIFY = F)
  do.call(rbind, dfs)
}

################################################################################
# Prints a message with all arguments explicitly or implicitly provided to 
# the parent function
# Requires this line inside the parent function: 
### parse_arguments(frmls=formals(),matchCall_expandFalse=match.call(expand.dots = F))

parse_arguments <- function(frmls, matchCall_expandFalse){
  # Get default argument values
  frmls <- as.list(frmls) 
  argument_df <- enframe(frmls, "argument", "value")
  
  # Get arguments from function call
  mtch <- as.list(matchCall_expandFalse)[-1] #Remove function name 
  match_df <- enframe(mtch, "argument", "value")
  
  # Merge default values and actual values
  df <- merge(argument_df, match_df, by = "argument", all.x=T)
  
  # Keep default value if an actual value not present
  df$value <- ifelse(!is.na(df$value.y), df$value.y, df$value.x)
  output_list <- deframe(df[,c("argument", "value")])
  
  # Convert all values to character and print as a message
  output_list <- lapply(output_list, as.character)
  message(paste(names(output_list),output_list,sep=":",collapse="\n"))
  return(unlist(output_list))
}
