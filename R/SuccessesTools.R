#' Get the distribution for a success/failure roll (White Wolf Style).
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param success An Integer, the minimum amount to be rolled to be considered a success. (Default: 6)
#' @param fail An Integer, the maximum number than if rolled, gets substracted from the success count. (Default: 1)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated in case of running an empyrical simulation. (Default: 10000)
#' @param theoretical Boolean, whether or not to use an ideal approach. Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8. (Default: FALSE)
#' @param plot.histogram Boolean, whether or not to plot a histogram with the data obtained
#' @return A list with either simulated or an exhaustive account of dice roll combinations success counts.
#' @examples
#' GetSuccessesDistribution(5)
#' GetSuccessesDistribution(X = 10, Y = 6, plot.histogram = FALSE)

GetSuccessesDistribution <- function (X, Y = 10, success = 6 , fail = 1,
                                      dice.rolls = 10000, theoretical = FALSE, 
                                      plot.histogram = TRUE){
	
  total <- GenerateSuccessesVector(X, Y, success, fail, dice.rolls, theoretical)

  if (plot.histogram){
    total.bars <- 2 * X  # Calculate number of bars based on dice rolled

    DiceRollHistogram(total, total.bars)
  }

  return(total)

}

#' Get the odds for a given amount of successes on a success/failure roll (White Wolf Style) to excede given threshod.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param odds An integer, the odds to beat.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param success An Integer, the minimum amount to be rolled to be considered a success. (Default: 6)
#' @param fail An Integer, the maximum number than if rolled, gets substracted from the success count. (Default: 1)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated in case of running an empyrical simulation. (Default: 10000)
#' @param theoretical Boolean, whether or not to use an ideal approach. Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8. (Default: FALSE)
#' @param plot.histogram Boolean, whether or not to plot a histogram with the data obtained
#' @return A list with either simulated or an exhaustive account of dice roll combinations success counts.
#' @examples
#' GetSuccessesDistribution(5, 2)
#' GetSuccessesDistribution(X = 10, odds = 4 Y = 6, plot.histogram = FALSE)

GetOddsSuccessesHigherThan <- function(X, odds, Y = 10, success = 6 , fail = 1,
                                      dice.rolls = 10000, theoretical = FALSE){

  total <- GenerateSuccessesVector(X, Y, success, fail, dice.rolls, theoretical)
  total.odds <- length(which(total >= odds))/length(total)

  return(total.odds * 100)

}

#' Get the odds for a given amount of successes on a success/failure roll (White Wolf Style) to miss given threshod.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param odds An integer, the odds not to beat.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param success An Integer, the minimum amount to be rolled to be considered a success. (Default: 6)
#' @param fail An Integer, the maximum number than if rolled, gets substracted from the success count. (Default: 1)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated in case of running an empyrical simulation. (Default: 10000)
#' @param theoretical Boolean, whether or not to use an ideal approach. Warning: An ideal approach can be very resource intensive for values of X and/or Y larger than 8. (Default: FALSE)
#' @param plot.histogram Boolean, whether or not to plot a histogram with the data obtained
#' @return A list with either simulated or an exhaustive account of dice roll combinations success counts.
#' @examples
#' GetSuccessesDistribution(5, 2)
#' GetSuccessesDistribution(X = 10, odds = 4 Y = 6, plot.histogram = FALSE)

GetOddsSuccessesLowerThan <- function(X, odds, Y = 10, success = 6 , fail = 1,
                                      dice.rolls = 10000, theoretical = FALSE){

  return(100 - GenerateSuccessesVector(X, odds, Y, success, fail, dice.rolls, theoretical))
  
}