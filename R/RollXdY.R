#TODO(documentation): Document using Roxygen

RollXdY <- function(X, Y = 10, Z = 10000, plot.histogram = TRUE, theoretical = FALSE){


  if (theoretical){
    total <- GetTheoretical(X,Y)
  } else {
    total <- GetEmpyrical(X,Y,Z)
  }

  if (plot.histogram){
    total.bars <- X * (Y-1)  # Calculate number of bars based on dice rolled

    h <- hist(total, breaks = total.bars)
    h$density = h$counts/sum(h$counts)*100
    plot(h,freq=FALSE)
  }

  return(total)
}
