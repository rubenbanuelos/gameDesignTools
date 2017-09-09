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

test_that("Success distribution graphs looks OK",{

  skip_if_not_installed(c("visualTest", "png"))  # Skip this test if Visual Test is not installed

  reference.graph <- ("./sampleGraphs/successref.png")  # Reference Graph path

  # Check if tests are ready to run

  skip_if_not(file.exists(reference.graph))  # Skip test if there's not a reference graph

  graph.name <- tempfile(fileext = ".png")

  reference.fingerprint <- getFingerprint(reference.graph) # Get Fingerprint for reference

  png(filename =  graph.name)
  GetSuccessesDistribution(5) #Graph sample
  dev.off()

  expect_true(isSimilar(graph.name, reference.fingerprint, threshold = 32))

})

test_that("Score distribution graphs look OK", {

  skip_if_not_installed(c("visualTest", "png"))  # Skip this test if Visual Test is not installed

  reference.graph <- ("./sampleGraphs/scoreref.png")  # Reference Graph path

  # Check if tests are ready to run

  skip_if_not(file.exists(reference.graph))  # Skip test if there's not a reference graph

  graph.name <- tempfile(fileext = ".png")

  reference.fingerprint <- getFingerprint(reference.graph)

  png(filename = graph.name)
  GetScoreDistribution(5) # Graph Sample
  dev.off()

  expect_true(isSimilar(graph.name, reference.fingerprint, threshold = 32))

})
