#' Choosing the best variable for splitting.
#' 
#' \code{BestForestSplit} searches through possible variables in order to find the most accurate split.
#' It returns the variable chosen, the model, and the two sets of fitted values where both 0 or 1 are considered a "success."
#'
#' \code{BestForestSplit} searches through possible variables to split using single variable logistic regression
#' with prior weights in the iteratively reweighted least squares procedure.  The variable minimizing residual deviance is chosen.  Note, this is a valid choice
#' since all models being compared are using the same Null Model containing only the intercept with equal weights.
#'
#' @param response Logical vector of 0 and 1 denoting the binomial response.
#' @param data A data frame or matrix consisting of all possible variables to attempt.
#' @param num.features A numeric of the number of variables in the dataset to possibly try.  The leftmost number of variables in the dataset are the variables chosen.
#' @param ntry A numeric of the number of variables from the \code{num.features} to attempt to split.  This is useful for building random forests.  For a standard tree, choose \code{ntry = num.features}.
#' @param weights A vector of weights for use in Weighted Least Squares.  Defaults to a vector of 1.
#' @return List of elements
#' \item{Feature}{Returns the variable chosen for best split.}
#' \item{fit}{A \code{glm} object of the fit with the chosen variable.}
#' \item{weights0}{A vector of the weights if response \code{0} was considered a success.  Calculated as \eqn{1 - weights1}.}
#' \item{weights1}{A vector of the weights if response \code{1} was considered a success.}
#' 
#' @export

BestForestSplit = function(response, data, num.features, ntry, weights = rep(1, nrow(data)))
{
  stopifnot(is.vector(response))
  if(sum(is.data.frame(data), is.matrix(data)) != 1) stop("Data must be matrix or data frame.")
  stopifnot(is.numeric(num.features))
  stopifnot(length(num.features) == 1)
  stopifnot(is.numeric(ntry))
  stopifnot(length(ntry) == 1)
  stopifnot(is.vector(weights))
  
  dev.vec = rep(NA, num.features)
  tryindex = sample(1:num.features, ntry, replace = FALSE)
  for(i in tryindex)
  {
    dev.vec[i] = suppressWarnings(glm(response ~ data[,i], family = "binomial", weights = weights)$deviance)
  }
  best.feature = which.min(dev.vec)
  fit = suppressWarnings(glm(response ~ data[,best.feature], family = "binomial", weights = weights))
  return(list(Feature = best.feature, fit = fit, weights0 = 1-fit$fitted.values, weights1 = fit$fitted.values))
}
