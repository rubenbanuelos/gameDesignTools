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
	
  if (theoretical){
    
    rolls <- GetDiceRollTheoreticalMatrix(X, Y)  # Gets a Dice Roll Ideal Matrix

  } else {

    rolls <- GetDiceRollEmpyricalMatrix(X, Y, dice.rolls)  # Gets a Dice Roll Empyrical Matrix

  }

  total = integer(0)

  for (i in 1:nrow(rolls)){
    successes <- length(which(rolls[i,] >= success))
    failures <- length(which(rolls[i,]<= fail))
  
    total <- c(total, successes - failures)
  }

  if (plot.histogram){
    total.bars <- 2 * X  # Calculate number of bars based on dice rolled

    DiceRollHistogram(total, total.bars)
  }

  return(total)

}

