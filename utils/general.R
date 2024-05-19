################################################################################
# Function that creates a list where every element is named
# based on the variable label of that element
# E.g. listN(iris) = list("iris" = iris)

listN <- function(...) {
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}
