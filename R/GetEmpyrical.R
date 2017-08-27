#TODO(documentation): Document using Roxygen

GetEmpyrical <- function(X, Y, Z){
		rolls <- 0

		for (i in 1:X){
     	rolls <- rolls + sample(1:Y , Z, replace = TRUE)  ##Roll Die
    }

    return(rolls)
}
