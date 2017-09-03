library("gameDesignTools")
context("Test dice success functions")

# Set test parameters
upper.bound <- 10
upper.bound.theoretical <- 8  #Troublesome if higher
test.samples <- 5

# Generate Random Inputs
x.vector             <- sample(2:upper.bound,             test.samples, replace = TRUE)
y.vector             <- sample(2:upper.bound,             test.samples, replace = TRUE)
x.vector.theoretical <- sample(2:upper.bound.theoretical, test.samples, replace = TRUE)
y.vector.theoretical <- sample(2:upper.bound.theoretical, test.samples, replace = TRUE)
dice.rolls.vector    <- sample(500:50000,                 test.samples, replace = TRUE)
