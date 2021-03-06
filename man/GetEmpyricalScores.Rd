% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiceRollers.R
\name{GetEmpyricalScores}
\alias{GetEmpyricalScores}
\title{Get a vector with simulated scores after rolling XdY}
\usage{
GetEmpyricalScores(X, Y, dice.rolls)
}
\arguments{
\item{X}{An integer, the amount of dice to be rolled.}

\item{Y}{An integer, the number of sides the die has. (Default: 10)}

\item{dice.rolls}{An Integer, the amount of dice rolls to be simulated.}
}
\value{
A vector with simulated values for dice roll scores.
}
\description{
Get a vector with simulated scores after rolling XdY
}
\examples{
GetEmpyricalScores(5, 10, 10000)
GetEmpyricalScores(X = 8, Y = 10, dice.rolls = 10000)
}
