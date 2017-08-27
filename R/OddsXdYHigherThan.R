#TODO(documentation): Document using Roxygen

OddsXdYHigherThan <- function(X, odds, Y = 10, theoretical = FALSE, Z = 10000){

  if (theoretical){
    rolls <- GetTheoretical(X, Y)  # Theoretical approach
  } else {
    rolls <- GetEmpyrical(X, Y, Z)  # Empyrical approach
  }

  total.odds <- length(which(rolls > odds))/length(rolls)

  return(total.odds * 100)
}

