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

OddsXdYLowerThan <- function(X, odds, Y = 10, theoretical = FALSE, Z = 10000){

  return((100 - OddsXdYHigherThan(X, odds, Y, theoretical, Z)))

}

GetScoreDistributionForXdY <- function(X, Y = 10, dice.rolls = 10000, plot.histogram = TRUE, theoretical = FALSE){


  if (theoretical){
    total <- GetTheoretical(X, Y)
    } else {
      total <- GetEmpyrical(X, Y, dice.rolls)
    }

    if (plot.histogram){
    total.bars <- X * (Y-1)  # Calculate number of bars based on dice rolled

    # Convert actual density to percentage values
    h <- hist(total, breaks = total.bars)
    h$density = h$counts/sum(h$counts)*100
    plot(h,freq=FALSE)

  }

  return(total)
}




GetTheoretical <- function(X, Y){
  # Warning: this function is very slow for high values for X and/or Y

  library(gtools)
  rolls <- rowSums(permutations(n = Y, r = X, repeats.allowed = TRUE))
  return(rolls)

}

GetEmpyrical <- function(X, Y, dice.rolls){
  total.score <- 0

  for (i in 1:X){
     	total.score <- total.score + sample(1:Y , dice.rolls, replace = TRUE)  ##Roll Die
     }

     return(rolls)
   }

