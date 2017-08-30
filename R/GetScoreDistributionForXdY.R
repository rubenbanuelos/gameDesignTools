#TODO(documentation): Document using Roxygen

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
