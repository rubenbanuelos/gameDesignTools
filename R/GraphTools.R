#' A simple histogram function unsing percentage values.
#' 
#' @param data A vector, the data to be graphed.
#' @param bars The amount of bars to show. (Default: 10)
#' @return A histogram with percentage values.
#' @examples
#' DiceRollHistogram(dice.roll.vector, bars = length(dice.roll.vector))
#' DiceRollHistogram(some.vector, bars = 5)

DiceRollHistogram <- function(data, bars){
  # Convert actual density to percentage values
  h <- hist(data, breaks = bars)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE)

  #TODO: Prettier Graphs
}