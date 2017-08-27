#TODO(documentation): Document using Roxygen

OddsXdYLowerThan <- function(X, odds, Y = 10, theoretical = FALSE, Z = 10000){

  return((100 - OddsXdYHigherThan(X, odds, Y, theoretical, Z)))

}
