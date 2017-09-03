#' Get the odds of rolling XdY dice and beating a given score.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param score.odds An integer, the score to beat.
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated in case of running an empyrical simulation. (Default: 10000)
#' @param theoretical Boolean, whether or not to use an ideal approach. Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8. (Default: FALSE)
#' @return An integer with the probability of the score given being achieved or beaten expressed as a percentage.
#' @examples
#' GetOddsXdYHigherThan(5, score.odds = 7)
#' GetOddsXdYHigherThan(X = 10, Y = 6, score.odds = 50)


GetOddsXdYHigherThan <- function(X, score.odds, Y = 10, theoretical = FALSE, dice.rolls = 10000){

  if (theoretical){
    total.score <- GetTheoreticalScores(X, Y)  # Theoretical approach
  } else {
    total.score <- GetEmpyricalScores(X, Y, dice.rolls)  # Empyrical approach
  }

  total.odds <- length(which(total.score > score.odds))/length(total.score)

  return(total.odds * 100)
}

#' Get the odds of rolling XdY dice and not beating a given score.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param score.odds An integer, the score not to beat.
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated in case of running an empyrical simulation. (Default: 10000)
#' @param theoretical Boolean, whether or not to use an ideal approach. Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8. (Default: FALSE)
#' @return An integer with the probability of the score given being achieved or beaten expressed as a percentage.
#' @examples
#' GetOddsXdYLowerThan(5, score.odds = 7)
#' GetOddsXdYLowerThan(X = 10, Y = 6, score.odds = 50)

GetOddsXdYLowerThan <- function(X, odds, Y = 10, theoretical = FALSE, Z = 10000){

  return((100 - OddsXdYHigherThan(X, odds, Y, theoretical, Z)))

}

#' Get the distribution for a total score after rolling XdY dice and summing the results.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An ingeger, the number of sides the die has. (Default: 10)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated in case of running an empyrical simulation. (Default: 10000)
#' @param theoretical Boolean, whether or not to use an ideal approach. Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8. (Default: FALSE)
#' @param plot.histogram Boolean, whether or not to plot a histogram with the data obtained
#' @return A list with either simulated or an exhaustive account of dice roll combinations scores.
#' @examples
#' GetScoreDistribution(5)
#' GetScoreDistribution(X = 10, Y = 6, plot.histogram = FALSE)

GetScoreDistribution <- function(X, Y = 10, dice.rolls = 10000, plot.histogram = TRUE, theoretical = FALSE){


  if (theoretical){
    total <- GetTheoreticalScores(X, Y)
  } else {
    total <- GetEmpyricalScores(X, Y, dice.rolls)
  }

  if (plot.histogram){
    total.bars <- X * (Y-1)  # Calculate number of bars based on dice rolled

    DiceRollHistogram(total, total.bars)
  }

  return(total)
}

#' Get a vector with all possible scores after rolling XdY.  Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8.
#'
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated.
#' @return A Vector with all possbile scores for XdY
#' @examples
#' GetTheoreticalScores(5, 10)
#' GetTheoreticalScores(X = 8, Y = 10)

GetTheoreticalScores <- function(X, Y){
  # Warning: this function is very slow for high values for X and/or Y

  library(gtools)
  total.score <- rowSums(permutations(n = Y, r = X, repeats.allowed = TRUE))
  return(total.score)

}

#' Get a vector with simulated scores after rolling XdY
#'
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated.
#' @return A vector with simulated values for dice roll scores.
#' @examples
#' GetEmpyricalScores(5, 10, 10000)
#' GetEmpyricalScores(X = 8, Y = 10, dice.rolls = 10000)

GetEmpyricalScores <- function(X, Y, dice.rolls){
  total.score <- 0

  for (i in 1:X){
    total.score <- total.score + sample(1:Y , dice.rolls, replace = TRUE)  ##Roll Die
  }

  return(total.score)
}
