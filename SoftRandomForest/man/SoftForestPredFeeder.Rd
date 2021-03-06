% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SoftForestPredFeeder.R
\name{SoftForestPredFeeder}
\alias{SoftForestPredFeeder}
\title{Choosing the appropriate depth function.}
\usage{
SoftForestPredFeeder(trainresponse, train, test, num.features, ntry, depth)
}
\arguments{
\item{trainresponse}{A vector of responses \code{0} and \code{1} for the training set with length equal to the number of observations in the training set.}

\item{train}{A matrix or data frame consisting of all possible variables to attempt for the training set.}

\item{test}{A matrix or data frame consisting of all possible variables to attempt for the test set.}

\item{num.features}{The number of variables in the dataset to possibly try.  The leftmost number of variables in the dataset are the variables chosen.}

\item{ntry}{The number of variables from the \code{num.features} to attempt to split.  This is useful for building random forests.  For a standard tree, choose \code{ntry = num.features}.}

\item{depth}{The number of the depth each SDT should be.  Here this ends with \eqn{2^{depth - 1}} terminal nodes.}
}
\value{
The output from the chosen function.
}
\description{
\code{SoftForestPredFeeder} is used inside the user interface function to choose the appropriate depth function since the SDT's depth are not generated dynamically.
}
\details{
\code{SoftForestPredDepth} chooses the correct of five possible depths that has functioning code.  Any invalid attempt returns an error.
}
