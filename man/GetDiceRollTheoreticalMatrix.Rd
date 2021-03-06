% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiceRollers.R
\name{GetDiceRollTheoreticalMatrix}
\alias{GetDiceRollTheoreticalMatrix}
\title{Get a M x N sized matrix (M = all possible combinatios of dice rolls and N = X) an exhaustive list of all possible combinations of dice rolls.}
\usage{
GetDiceRollTheoreticalMatrix(X, Y)
}
\arguments{
\item{X}{An integer, the amount of dice to be rolled.}

\item{Y}{An integer, the number of sides the die has. (Default: 10)}
}
\value{
A Matrix with all possible dice rolls.
}
\description{
Get a M x N sized matrix (M = all possible combinatios of dice rolls and N = X) an exhaustive list of all possible combinations of dice rolls.
}
\examples{
GetDiceRollEmpyricalMatrix(5, 10)
GetDiceRollEmpyricalMatrix(X = 8, Y = 10)
}
