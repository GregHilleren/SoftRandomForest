#' Choosing the appropriate depth function.
#' 
#' \code{SoftForestPredFeeder} is used inside the user interface function to choose the appropriate depth function since the SDT's depth are not generated dynamically.
#' 
#' \code{SoftForestPredDepth} chooses the correct of five possible depths that has functioning code.  Any invalid attempt returns an error.
#' 
#' @param trainresponse A vector of responses \code{0} and \code{1} for the training set with length equal to the number of observations in the training set.
#' @param train A matrix or data frame consisting of all possible variables to attempt for the training set.
#' @param test A matrix or data frame consisting of all possible variables to attempt for the test set.
#' @param num.features The number of variables in the dataset to possibly try.  The leftmost number of variables in the dataset are the variables chosen.
#' @param ntry The number of variables from the \code{num.features} to attempt to split.  This is useful for building random forests.  For a standard tree, choose \code{ntry = num.features}.
#' @param depth The number of the depth each SDT should be.  Here this ends with \eqn{2^{depth - 1}} terminal nodes.
#' @return The output from the chosen function.

SoftForestPredFeeder = function(trainresponse, train, test, num.features, ntry, depth)
{
  stopifnot(is.vector(trainresponse))
  if(sum(is.data.frame(train), is.matrix(train)) != 1) stop("Training data must be matrix or data frame.")
  if(sum(is.data.frame(test), is.matrix(test)) != 1) stop("Test data must be matrix or data frame.")
  stopifnot(is.numeric(num.features))
  stopifnot(length(num.features) == 1)
  stopifnot(is.numeric(ntry))
  stopifnot(length(ntry) == 1)
  stopifnot(is.numeric(depth))
  stopifnot(length(depth) == 1)
  
  if (depth == 1) SoftForestPredDepth1(trainresponse, train, test, num.features, ntry)
  else if (depth == 2) SoftForestPredDepth2(trainresponse, train, test, num.features, ntry)
  else if (depth == 3) SoftForestPredDepth3(trainresponse, train, test, num.features, ntry)
  else if (depth == 4) SoftForestPredDepth4(trainresponse, train, test, num.features, ntry)
  else if (depth == 5) SoftForestPredDepth5(trainresponse, train, test, num.features, ntry)
  else message("Not a Possible Depth")
}
