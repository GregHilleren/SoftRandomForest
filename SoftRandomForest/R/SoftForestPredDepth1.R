#' Building a single level for the Random Forest of SDTs.
#' 
#' @aliases SoftForestPredDepth1 SoftForestPredDepth2 SoftForestPredDepth3 SoftForestPredDepth4 SoftForestPredDepth5
#' 
#' \code{SoftForestPredDepth} creates a single SDT for only classification to build the Random Forest of SDTs.  This is equivalent to a single tree in a standard Random Forest.
#'
#' \code{SoftForestPredDepth} runs through the \code{BestForestSplit} function for each node given sets of weights determined at previous steps
#' in order to fit to determine a single classification SDT with a prespecified depth.  The final prediction returned is a weighted average
#' of the terminal nodes for a probability that a single observation is coded \code{1} which would be in the current class.
#' 
#' @param trainresponse A vector of responses \code{0} and \code{1} for the training set with length equal to the number of observations in the training set.
#' @param train A matrix or data frame consisting of all possible variables to attempt for the training set.
#' @param test A matrix or data frame consisting of all possible variables to attempt for the test set.
#' @param num.features The number of variables in the dataset to possibly try.  The leftmost number of variables in the dataset are the variables chosen.
#' @param ntry The number of variables from the \code{num.features} to attempt to split.  This is useful for building random forests.  For a standard tree, choose \code{ntry = num.features}.
#' @param keep Logical if weights from a single observation should be kept.  Keep FALSE if a Random Forest is to be built.
#' @return A vector of the final fitted probabilities for this classification.
#'
#' @importFrom boot inv.logit
#' @importFrom stats glm
#' @importFrom utils write.csv

SoftForestPredDepth1 = function(trainresponse, train, test, num.features, ntry, keep = FALSE)
{
  stopifnot(is.vector(trainresponse))
  if(sum(is.data.frame(train), is.matrix(train)) != 1) stop("Training data must be matrix or data frame.")
  if(sum(is.data.frame(test), is.matrix(test)) != 1) stop("Test data must be matrix or data frame.")
  stopifnot(is.numeric(num.features))
  stopifnot(length(num.features) == 1)
  stopifnot(is.numeric(ntry))
  stopifnot(length(ntry) == 1)
  
  Response01 = BestForestSplit(trainresponse, train, num.features, ntry)

  Predweight01 =  as.numeric(inv.logit(Response01$fit$coefficients[1] + Response01$fit$coefficients[2]*test[,Response01$Feature]))
 
  Prediction = Predweight01
  
  if(keep == FALSE) return(Prediction)
  if(keep == TRUE) return(list(Prediction = Prediction, AllFeatures = cbind(Response01$Feature), AllWeights = cbind(Predweight01)))
}
