library("gameDesignTools")
context("Test dice score functions")

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

test_that("Odds are correct",{

	for (i in 1:test.samples){

		# Commence tests

		#Empyrical

		# Test for impossible odds
		expect_equal(GetOddsXdYHigherThan(x.vector[i], (x.vector[i] * y.vector[i] + 1),
								 y.vector[i], FALSE, dice.rolls.vector[i]), 0)

		# Test for trivial odds
		expect_equal(GetOddsXdYHigherThan(x.vector[i], (x.vector[i]),y.vector[i],
								 FALSE, dice.rolls.vector[i]), 100)

		#Theoretical

		# Test for impossible odds
		expect_equal(GetOddsXdYHigherThan(x.vector.theoretical[i],
								(x.vector.theoretical[i] * y.vector.theoretical[i] + 1),
								 y.vector.theoretical[i], TRUE, dice.rolls.vector[i]), 0)

		# Test for trivial odds
		expect_equal(GetOddsXdYHigherThan(x.vector.theoretical[i],
								(x.vector.theoretical[i]),y.vector.theoretical[i],
								 TRUE, dice.rolls.vector[i]), 100)

		
	}

})

test_that("Distribution works", {

	for (i in 1:test.samples){

		# Empyrical

		# Get an empyrical distribution
		a <- GetScoreDistribution(x.vector[i], y.vector[i], dice.rolls.vector[i], FALSE, FALSE)

		# Test values are within expected ranges
		expect_equal(length(a), dice.rolls.vector[i])
		expect_true(max(a) <= y.vector[i] * x.vector[i])
		expect_true(min(a) >= x.vector[i])  #These tests are more lax since they use empyrical


		# Theoretical

		# Get an theoretical distribution
		a <- GetScoreDistribution(x.vector.theoretical[i], y.vector.theoretical[i],
														  dice.rolls.vector[i], FALSE, TRUE)

		# Test values are within expected ranges
		expect_equal(max(a), y.vector.theoretical[i] * x.vector.theoretical[i])
		expect_equal(min(a), x.vector.theoretical[i])
	}

})
