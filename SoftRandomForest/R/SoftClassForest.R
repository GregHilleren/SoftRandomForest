#' Implementing a Random Forest of SDTs.
#' 
#' \code{SoftClassForest} creates categorical Random Forests of Soft Decision Trees while returning
#' the fitted classification given by the majority vote of individual SDTs.
#'
#' \code{SoftClassForest} individually fits a Random Forest for each possible classification response using \code{SoftForestPredFeeder} function
#' one classification at a time.  The result from each one of these SDTs is a fitted probability of \code{0} or \code{1}.
#' Once all classifications have a fitted probability, the observation is classified as the maximum a posteriori probability.
#' Given a Random Forest of SDTs, the final Random Forest classification goes to the majority vote from the SDTs.
#' 
#' @param trainresponses A matrix or data frame of responses \code{0} and \code{1} for the training set with length equal to the number of observations in the training set and width equal to the number of possible classifications.
#' @param train A matrix or data frame consisting of all possible variables to attempt for the training set.
#' @param test A matrix or data frame consisting of all possible variables to attempt for the test set.
#' @param ntry A numeric of the number of variables from the \code{num.features} to attempt to split.  This is useful for building random forests.  For a standard tree, choose \code{ntry = num.features}.
#' @param ntrees A numeric of the number of SDTs to build in the Random Forest.
#' @param depth A numeric of the number of the depth each SDT should be.  Here this ends with \eqn{2^{depth - 1}} terminal nodes.
#' @param bag Logical if Random Forests should be built with bootstrap aggregating (TRUE) or raw data (FALSE).
#' @return A vector of the final classifications based on the Random Forest generated.
#' 
#' @export
#' 
#' @examples
#' Responses = SoftClassMatrix(as.vector(iris$Species))
#' SoftClassForest(trainresponses = Responses, train = iris[,1:4], test = iris[,1:4], 
#' ntry = 2, ntrees = 15, depth = 2, bag = TRUE)

SoftClassForest = function(trainresponses, train, test, ntry, ntrees, depth, bag = TRUE)
{
  if(sum(is.data.frame(trainresponses), is.matrix(trainresponses), is.vector(trainresponses)) != 1) stop("Responses must be matrix or data frame.")
  if(sum(is.data.frame(train), is.matrix(train)) != 1) stop("Training data must be a matrix or data frame.")
  if(sum(is.data.frame(test), is.matrix(test)) != 1) stop("Test data must be a matrix or data frame.")
  stopifnot(is.numeric(ntry))
  stopifnot(length(ntry) == 1)
  stopifnot(is.numeric(ntrees))
  stopifnot(length(ntrees) == 1)
  stopifnot(is.numeric(depth))
  stopifnot(length(depth) == 1)
  if(sum(bag == TRUE, bag == FALSE) != 1) stop("Paramater bag must be either TRUE or FALSE")
  if(ncol(train) != ncol(test)) stop("Must be equal number of columns in the training and test set.")
  if(nrow(train) != nrow(trainresponses)) stop("Number of observations in Training set and Training responses must be equal.")
  if(ntry > ncol(train)) stop("Cannot attempt to try more variables than exist.")
  
  classes = ncol(trainresponses)
  num.features = ncol(train)
  softvotes = matrix(NA, nrow = nrow(test), ncol = ntrees)
  for(i in 1:ntrees)
  {
    softpredmatrix = matrix(NA, nrow = nrow(test), ncol = classes)
    if(bag == TRUE) index = sample(1:nrow(trainresponses), nrow(trainresponses), replace = TRUE)
    if(bag == FALSE) index = 1:nrow(trainresponses)
    for(j in 1:classes)
    {
      softpredmatrix[,j] = SoftForestPredFeeder(trainresponses[index,j], train[index,], test, num.features, ntry, depth)
    }
    softprediction = rep(NA, nrow(test))
    for(j in 1:nrow(softpredmatrix))
    {
      softprediction[j] = which.max(softpredmatrix[j,])
    }
    softvotes[,i] = softprediction
  }
  finalprediction = rep(NA, nrow(test))
  for(j in 1:nrow(softvotes))
  {
    finalprediction[j] = as.numeric(ClassMode(softvotes[j,]))
  }
  return(finalprediction)
}
