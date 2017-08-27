RollXdY <- function(X, Y = 10, Z = 10000, plot.histogram = T){
  
  
  total <-  0
  
  for (i in 1:X){
    total <-total + sample(1:Y , Z, replace = T)  ##Roll Die
  }
  
  if (plot.histogram){
    total.bars <- X * (Y-1)  # Calculate number of bars based on dice rolled
  
    h <- hist(total, breaks = total.bars)
    h$density = h$counts/sum(h$counts)*100
    plot(h,freq=F)
  }
  
  return(total)
}