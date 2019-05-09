#' Converting response vector to sparse matrix.
#' 
#' \code{SoftClassMatrix} converts a classification response matrix into a sparse matrix that can be used for inputs
#' into the \code{SoftRandomForest} function.
#'
#' \code{SoftClassMatrix} runs through each line of a classification vector and creates a sparse matrix where each column represents
#' an individual classification.  The output matrix has number of rows equal to the number of rows of the input vector and number of columns
#' equal to the number of unique entries in the input vector.  The order is determined by the order they appear in the vector.
#' Adjust this afterwards if another order is desired.
#' 
#' @param responses A vector of classification responses.
#' @param classes A vector of possible classifications with a manually specified order.  Must contain all elements in the responses vector.
#' @return A matrix where \code{1} indicates that observation was classified as that column's response and \code{0} if not. 
#' 
#' @export
#' 
#' @examples 
#' Input = c("A", "C", "B", "B", "A", "B")
#' SoftClassMatrix(Input, classes = c("A", "B", "C", "D"))

SoftClassMatrix = function(responses, classes = NA)
{
  stopifnot(is.vector(responses))
  if(sum(is.na(classes)) > 0) classes = unique(responses)
  if(sum(responses %in% classes) != length(responses)) stop("Some entry in the responses is not in the possible classifications.")
  
  responseMat = matrix(NA, nrow = length(responses), ncol = length(classes))
  for(i in 1:length(responses))
  {
    for(j in 1:length(classes))
    {
      responseMat[i,j] = if(responses[i] == classes[j]) 1 else 0
    }
  }
  colnames(responseMat) = classes
  return(as.matrix(responseMat))
}
