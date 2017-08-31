GetSuccessesDistribution <- function (X, Y = 10, success, dice.rolls = 10000, theoretical = FALSE){
	
  if (theoretical){
    
    rolls <- GetDiceRollTheoreticalMatrix(X, Y)  # Gets a Dice Roll Ideal Matrix

    return(rolls)

  } else {

    rolls <- GetDiceRollEmpyricalMatrix(X, Y, dice.rolls)  # Gets a Dice Roll Empyrical Matrix
    
    return(rolls)

  }

}

GetDiceRollEmpyricalMatrix <- function (X, Y, dice.rolls){
	roll.list <- list()

	for (i in 1:X){
    roll.list[[i]] <- sample(1:Y, dice.rolls, replace = TRUE)  # Roll Die

  }

  roll.matrix <- do.call(cbind, roll.list)
  return(roll.matrix)
}

GetDiceRollTheoreticalMatrix <- function (X, Y){

	library(gtools)
  roll.matrix <- permutations(n = Y, r = X, repeats.allowed = TRUE)
  return(roll.matrix)

}
