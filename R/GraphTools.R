DiceRollHistogram <- function(data, bars){
    # Convert actual density to percentage values
    h <- hist(data, breaks = bars)
    h$density = h$counts/sum(h$counts)*100
    plot(h,freq=FALSE)

    #TODO: Prettier Graphs
}