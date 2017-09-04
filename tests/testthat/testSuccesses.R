library("gameDesignTools")
context("Test dice success functions")

# Set test parameters
upper.bound <- 10
upper.bound.theoretical <- 7  # Troublesome if higher
test.samples <- 5

# Generate Random Inputs
x.vector             <- sample(3:upper.bound,             test.samples, replace = TRUE)
y.vector             <- sample(3:upper.bound,             test.samples, replace = TRUE)
x.vector.theoretical <- sample(3:upper.bound.theoretical, test.samples, replace = TRUE)
y.vector.theoretical <- sample(3:upper.bound.theoretical, test.samples, replace = TRUE)
dice.rolls.vector    <- sample(500:50000,                 test.samples, replace = TRUE)

test_that("Odds are correct", {

	for (i in 1:test.samples){

		# Empyrical

		# Impossible odds
		expect_equal(GetOddsSuccessesHigherThan(x.vector[i], (x.vector[i] + 1), y.vector[i],
																						ceiling((y.vector[i] / 2)), 1, dice.rolls.vector[i], FALSE),
								 0)

		# Trivial odds
		expect_equal(GetOddsSuccessesHigherThan(x.vector[i], 0, y.vector[i], ceiling((y.vector[i] / 2)),
																						1, dice.rolls.vector[i], FALSE),
								 100)

		# Theoretical

		# Impossible odds
		expect_equal(GetOddsSuccessesHigherThan(x.vector.theoretical[i], (x.vector.theoretical[i] + 1),
																						y.vector.theoretical[i],	ceiling((y.vector.theoretical[i] / 2)), 
																						1, dice.rolls.vector[i], TRUE),
								 0)

		# Trivial odds
		expect_equal(GetOddsSuccessesHigherThan(x.vector.theoretical[i], 0, y.vector.theoretical[i],
																						ceiling((y.vector.theoretical[i] / 2)), 1, dice.rolls.vector[i], TRUE),
								 100)
	}

})

test_that("Distribution works", {

	for (i in 1:test.samples){

		# Empyrical

		# Get an empyrical distribution
		a <- GetSuccessesDistribution (x.vector[i], y.vector[i], ceiling((y.vector[i] / 2)), 1,
                                   dice.rolls.vector[i], FALSE, FALSE)

		# Test values are within expected ranges
		expect_equal(length(a), dice.rolls.vector[i])
		expect_true(max(a) <= x.vector[i])
		expect_true(min(a) >= -x.vector[i])  #These tests are more lax since they use empyrical


		# Theoretical

		# Get an theoretical distribution
		a <- GetSuccessesDistribution (x.vector.theoretical[i], y.vector.theoretical[i], 
			ceiling((y.vector.theoretical[i] / 2)), 1, dice.rolls.vector[i], TRUE, FALSE)

		# Test values are within expected ranges
		expect_equal(max(a), x.vector.theoretical[i])
		expect_equal(min(a), -x.vector.theoretical[i])
	}

})

