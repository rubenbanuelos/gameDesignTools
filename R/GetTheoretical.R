#TODO(documentation): Document using Roxygen

GetTheoretical <- function(X, Y){
  # Warning: this function is very slow for high values for X and/or Y

  library(gtools)
  rolls <- rowSums(permutations(n = Y, r = X, repeats.allowed = TRUE))
  return(rolls)

}
