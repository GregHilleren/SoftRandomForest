#' Recording the prediction weights to analyze observation-level patterns
#' 
#' \code{SoftObservation} runs the depth given of a single SDT of a single response focusing on a single (or a few) observations
#' in order to make inference from the Prediction Weights found.
#'
#' \code{SoftObservation} runs the internal SoftForestPredDepth functions so that a single SDT's weights can be recorded.
#' This can then be exported to any tree visual representation to see how the observation(s) of interest pass through the SDT
#' This follows from the other user interface function SoftClassForest where the test set is the observation of interest
#' instead of being used for testing misclassification.  Exporting these weights for visual representation is possible and recommended.
#' 
#' @param response A vector of responses \code{0} and \code{1} for a single classification for the training set with length equal to the number of observations in the training set and width \code{1}.
#' @param responselabel A character string of the title of the response variable used.
#' @param train A matrix or data frame consisting of the entire dataset to train.
#' @param depth A numeric of the number of the depth each SDT should be.  Here this ends with \eqn{2^{depth - 1}} terminal nodes.
#' @param keep A logical passed to the internal functions to keep the prediction and weights (TRUE) or discard the weights and keep only prediction (FALSE).
#' @param observation A numeric or vector containing observations of interest to keep the fitted prediction probability and weights.
#' @param export A logicial indicating if results should be printed directly (FALSE) or exported to csv (TRUE).
#' @param path A directory location to save the exported csv file.  Defaults to the working directory if left blank.
#' @return A list of possible elements
#' \item{Prediction}{A vector of fitted probabilities for the given classification and observation(s).}
#' \item{AllFeatures}{A numeric list of Features chosen at each node where the number represents the column number in the data.}
#' \item{AllWeights}{A matrix of weights where the rows represent the observations and columns represent the weights used at different stages.} 
#' \item{SoftObservationDataOutput.csv}{If export = TRUE, this csv file can be used with an Excel supplement to create visual displays of a single observation for a single response.}
#' 
#' @export

SoftObservation = function(response, responselabel = "No Variable Label", train, depth, keep = TRUE, observation, export = FALSE, path = NA)
{
  stopifnot(is.vector(response))
  stopifnot(is.character(responselabel))
  if(sum(is.data.frame(train), is.matrix(train)) != 1) stop("Training data must be matrix or data frame.")
  stopifnot(is.numeric(depth))
  stopifnot(length(depth) == 1)
  stopifnot(is.vector(observation))
  stopifnot(sum(export == TRUE, export == FALSE) == 1)
  stopifnot(sum(is.character(path) == TRUE, is.na(path) == TRUE) == 1)
  
  ntry = ncol(train)
  if(length(observation) == 1) testdata = as.matrix(t(train[observation,]))
  if(length(observation) != 1) testdata = as.matrix(train[observation,])
  ntrees = 1
  num.features = ntry
  if (depth == 1) ObservationOutput = SoftForestPredDepth1(response, train, testdata, num.features, ntry, keep)
  else if (depth == 2) ObservationOutput = SoftForestPredDepth2(response, train, testdata, num.features, ntry, keep)
  else if (depth == 3) ObservationOutput = SoftForestPredDepth3(response, train, testdata, num.features, ntry, keep)
  else if (depth == 4) ObservationOutput = SoftForestPredDepth4(response, train, testdata, num.features, ntry, keep)
  else if (depth == 5) ObservationOutput = SoftForestPredDepth5(response, train, testdata, num.features, ntry, keep)
  else print("Not a Possible Depth")
  
  if (export == FALSE) return(ObservationOutput)
  if (export == TRUE)
  {
    AllNames = c("Variable01", "Variable11", "Variable12", "Variable21", "Variable22", "Variable23", "Variable24", "Variable31", "Variable32", "Variable33", "Variable34", "Variable35", "Variable36", "Variable37", "Variable38", "Variable41", "Variable42", "Variable43", "Variable44", "Variable45", "Variable46", "Variable47", "Variable48", "Variable49", "Variable410", "Variable411", "Variable412", "Variable413", "Variable414", "Variable415", "Variable416")
    NumTermNodes = length(ObservationOutput$AllFeatures)
    colnames(ObservationOutput$AllFeatures) = AllNames[1:NumTermNodes]
    if(is.null(colnames(train)) == FALSE)
    {
      for(i in 1:length(ObservationOutput$AllFeatures))
      {
        ObservationOutput$AllFeatures[i] = colnames(train)[as.numeric(ObservationOutput$AllFeatures[i])]
      }
    }
    temp = unlist(lapply(FUN = as.data.frame, ObservationOutput))
    output = cbind(temp$Prediction, temp$AllFeatures, temp$AllWeights)
    names(output)[1] = "Prediction"
    OutputLength = length(output)
    output[OutputLength + 1] = observation
    names(output)[OutputLength + 1] = "Observation"
    output[OutputLength + 2] = responselabel
    names(output)[OutputLength + 2] = "Response"
    if(is.na(path) == TRUE) path = getwd()
    file = paste(path, "/SoftObservationDataOutput.csv", sep = "")
    write.csv(t(output), file)
  }
}
