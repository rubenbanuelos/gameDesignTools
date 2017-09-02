library("gameDesignTools")
context("Test Score Distributions")

test_that("Empyrical scores make sense", {
    #Generate Random Inputs for Empyrical
    x.vector <- sample(1:10, 5, replace = TRUE)
    y.vector <- sample(1:10, 5, replace = TRUE)
    dice.rolls.vector <- sample(500:50000, 5, replace = TRUE)

    for (i in 1:5){
      a <- GetEmpyricalScores(x.vector[i], y.vector[i], dice.rolls.vector[i])

      # Commence tests
      # expect_true(length(a) == dice.rolls.vector[i])  # alternate version
      expect_equal(length(a), dice.rolls.vector[i])
      

      expect_true(all(a >= x.vector[i]))
      expect_true(all(a <= x.vector[i] * y.vector[i]))

    }

  })

