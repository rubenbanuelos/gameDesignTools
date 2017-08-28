#TODO(documentation): Document using Roxygen

GetEmpyrical <- function(X, Y, dice.rolls){
		total.score <- 0

		for (i in 1:X){
     	total.score <- total.score + sample(1:Y , dice.rolls, replace = TRUE)  ##Roll Die
    }

    return(rolls)
}
