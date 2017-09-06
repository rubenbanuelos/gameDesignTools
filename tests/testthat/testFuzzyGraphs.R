library("gameDesignTools")
library("visualTest")

context("Test graph using fuzzy graph comparisson")

# Set test parameters
upper.bound <- 10
upper.bound.theoretical <- 6  # Troublesome if higher
test.samples <- 5

# Generate Random Inputs
x.vector             <- sample(3:upper.bound,             test.samples, replace = TRUE)
y.vector             <- sample(3:upper.bound,             test.samples, replace = TRUE)
x.vector.theoretical <- sample(3:upper.bound.theoretical, test.samples, replace = TRUE)
y.vector.theoretical <- sample(3:upper.bound.theoretical, test.samples, replace = TRUE)
dice.rolls.vector    <- sample(500:50000,                 test.samples, replace = TRUE)
