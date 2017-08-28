#TODO(documentation): Document using Roxygen

OddsXdYHigherThan <- function(X, score.odds, Y = 10, theoretical = FALSE, dice.rolls = 10000){

  if (theoretical){
    total.score <- GetTheoretical(X, Y)  # Theoretical approach
  } else {
    total.score <- GetEmpyrical(X, Y, dice.rolls)  # Empyrical approach
  }

  total.odds <- length(which(total.score > score.odds))/length(total.score)

  return(total.odds * 100)
}

