


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

#' Get a M x N sized matrix (M = dice.rolls and N = X) with M simulations of dice rolls.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @param dice.rolls An Integer, the amount of dice rolls to be simulated.
#' @return A Matrix with simulated dice rolls.
#' @examples
#' GetDiceRollEmpyricalMatrix(5, 10, 10000)
#' GetDiceRollEmpyricalMatrix(X = 8, Y = 10, dice.rolls = 10000)

GetDiceRollEmpyricalMatrix <- function (X, Y, dice.rolls){
	roll.list <- list()

	for (i in 1:X){
    roll.list[[i]] <- sample(1:Y, dice.rolls, replace = TRUE)  # Roll Die

  }

  roll.matrix <- do.call(cbind, roll.list)
  return(roll.matrix)
}

#' Get a M x N sized matrix (M = all possible combinatios of dice rolls and N = X) an exhaustive list of all possible combinations of dice rolls.
#' 
#' @param X An integer, the amount of dice to be rolled.
#' @param Y An integer, the number of sides the die has. (Default: 10)
#' @return A Matrix with all possible dice rolls.
#' @examples
#' GetDiceRollEmpyricalMatrix(5, 10)
#' GetDiceRollEmpyricalMatrix(X = 8, Y = 10)

GetDiceRollTheoreticalMatrix <- function (X, Y){

	library(gtools)
  roll.matrix <- permutations(n = Y, r = X, repeats.allowed = TRUE)
  return(roll.matrix)

}

