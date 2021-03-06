% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiceRollers.R
\name{GetDiceRollEmpyricalMatrix}
\alias{GetDiceRollEmpyricalMatrix}
\title{Get a M x N sized matrix (M = dice.rolls and N = X) with M simulations of dice rolls.}
\usage{
GetDiceRollEmpyricalMatrix(X, Y, dice.rolls)
}
\arguments{
\item{X}{An integer, the amount of dice to be rolled.}

\item{Y}{An integer, the number of sides the die has. (Default: 10)}

\item{dice.rolls}{An Integer, the amount of dice rolls to be simulated.}
}
\value{
A Matrix with simulated dice rolls.
}
\description{
Get a M x N sized matrix (M = dice.rolls and N = X) with M simulations of dice rolls.
}
\examples{
GetDiceRollEmpyricalMatrix(5, 10, 10000)
GetDiceRollEmpyricalMatrix(X = 8, Y = 10, dice.rolls = 10000)
}
