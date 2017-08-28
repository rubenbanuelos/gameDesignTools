GetSuccessesDistribution <- function (X, Y, Z, dice.rolls = 10000){
	#TODO Theoretical


}

GetDiceRollEmpyricalMatrix <- function (X, Y, Z, dice.rolls){
	roll.list <- list()

	for (i in 1:X){
    roll.list[[i]] <- sample(1:Y, dice.rolls, replace = TRUE)  # Roll Die

  }

  roll.matrix <- do.call(rbind, roll.matrix)
  return roll.matrix
}

GetDiceRollEmpyricalMatrix <- function (X, Y, Z){

	library(gtools)
  roll.matrix <- permutations(n = Y, r = X, repeats.allowed = TRUE)
  return(roll.matrix)

}