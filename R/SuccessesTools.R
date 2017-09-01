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
