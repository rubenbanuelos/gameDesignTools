library("gameDesignTools")
context("Test dice rolls generators")

test_that("Empyrical score dice rolls make sense", {
    #Generate Random Inputs for Empyrical
    x.vector <- sample(1:10, 5, replace = TRUE)
    y.vector <- sample(1:10, 5, replace = TRUE)
    dice.rolls.vector <- sample(500:50000, 5, replace = TRUE)

    for (i in 1:5){
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
    #Generate Random Inputs for Empyrical
    x.vector <- sample(1:8, 5, replace = TRUE)
    y.vector <- sample(1:8, 5, replace = TRUE)
    
    for (i in 1:5){
      a <- GetTheoreticalScores(x.vector[i], y.vector[i])
      # Commence tests
      
      #Lower score should never be below amount of dice rolled
      expect_true(all(a >= x.vector[i]))
      
      #Highest score should never be above the sum of the highest rolls possible
      expect_true(all(a <= x.vector[i] * y.vector[i]))

    }

  })

