library("gameDesignTools")
context("Test dice rolls generators")

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


test_that("Empyrical score dice rolls make sense", {


  for (i in 1:test.samples){
    a <- GetEmpyricalScores(x.vector[i], y.vector[i], dice.rolls.vector[i])

    # Commence tests


    # Check that the right amount of dice were rolled
    # expect_true(length(a) == dice.rolls.vector[i])  # alternate version
    expect_identical(length(a), dice.rolls.vector[i])

    #Lower score should never be below amount of dice rolled
    expect_true(all(a >= x.vector[i]))

    #Highest score should never be above the sum of the highest rolls possible
    expect_true(all(a <= x.vector[i] * y.vector[i]))

  }

})

test_that("Theoretical score dice rolls make sense", {

  for (i in 1:test.samples){

    a <- GetTheoreticalScores(x.vector.theoretical[i], y.vector.theoretical[i])

    # Commence tests

    #Lower score should never be below amount of dice rolled
    expect_true(all(a >= x.vector.theoretical[i]))

    #Highest score should never be above the sum of the highest rolls possible
    expect_true(all(a <= x.vector.theoretical[i] * y.vector.theoretical[i]))

  }

})

test_that("Empyrical success dice rolls make sense", {

  for (i in 1:test.samples){

    a <- GetDiceRollEmpyricalMatrix(x.vector[i], y.vector[i], dice.rolls.vector[i])

    # Commence tests

    # Check that vector dimension match input data
    expect_identical(nrow(a), dice.rolls.vector[i])
    expect_identical(ncol(a), x.vector[i])

    # Check that maximum value matches input

    expect_identical(max(a), y.vector[i])
  }

})

test_that("Theoretical success dice rolls make sense", {

  for (i in 1:test.samples){

    a <- GetDiceRollTheoreticalMatrix(x.vector.theoretical[i], y.vector.theoretical[i])

    # Commence tests

    # Check that vector dimension match input data
    expect_identical(ncol(a), x.vector.theoretical[i])

    # Check that maximum value matches input

    expect_identical(max(a), y.vector.theoretical[i])
  }

})

test_that("Success vector generator works", {

  for (i in 1:test.samples){
    # Commence tests

    #Result should be zero successes
    a <- GenerateSuccessesVector(x.vector[i], y.vector[i], (y.vector[i] + 1), 0, dice.rolls.vector[i], FALSE)
    expect_true(all(a == 0))
    #Result should be all fails
    a <- GenerateSuccessesVector(x.vector[i], y.vector[i], (y.vector[i] + 1), y.vector[i], dice.rolls.vector[i], FALSE)
    expect_true(all(a == -x.vector[i]))
  }

})
