#' @export

SoftForestPredDepth3 = function(trainresponse, train, test, num.features, ntry, keep = FALSE)
{
  stopifnot(is.vector(trainresponse))
  if(sum(is.data.frame(train), is.matrix(train)) != 1) stop("Training data must be matrix or data frame.")
  if(sum(is.data.frame(test), is.matrix(test)) != 1) stop("Test data must be matrix or data frame.")
  stopifnot(is.numeric(num.features))
  stopifnot(length(num.features) == 1)
  stopifnot(is.numeric(ntry))
  stopifnot(length(ntry) == 1)
  
  Response01 = BestForestSplit(trainresponse, train, num.features, ntry)
  Response11 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0)
  Response12 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1)
  Response21 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0*Response11$weights0)
  Response22 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0*Response11$weights1)
  Response23 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1*Response11$weights0)
  Response24 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1*Response11$weights1)
  
  Predweight01 =  as.numeric(inv.logit(Response01$fit$coefficients[1] + Response01$fit$coefficients[2]*test[,Response01$Feature]))
  Predweight11 =  as.numeric(inv.logit(Response11$fit$coefficients[1] + Response11$fit$coefficients[2]*test[,Response11$Feature]))
  Predweight12 =  as.numeric(inv.logit(Response12$fit$coefficients[1] + Response12$fit$coefficients[2]*test[,Response12$Feature]))
  Predweight21 =  as.numeric(inv.logit(Response21$fit$coefficients[1] + Response21$fit$coefficients[2]*test[,Response21$Feature]))
  Predweight22 =  as.numeric(inv.logit(Response22$fit$coefficients[1] + Response22$fit$coefficients[2]*test[,Response22$Feature]))
  Predweight23 =  as.numeric(inv.logit(Response23$fit$coefficients[1] + Response23$fit$coefficients[2]*test[,Response23$Feature]))
  Predweight24 =  as.numeric(inv.logit(Response24$fit$coefficients[1] + Response24$fit$coefficients[2]*test[,Response24$Feature]))

  Prediction = (1-Predweight01)*(1-Predweight11)*Predweight21 + (1-Predweight01)*Predweight11*Predweight22 + Predweight01*(1-Predweight12)*Predweight23 + Predweight01*Predweight12*Predweight24 
  
  if(keep == FALSE) return(Prediction)
  if(keep == TRUE) return(list(Prediction = Prediction, AllFeatures = cbind(Response01$Feature, Response11$Feature, Response12$Feature, Response21$Feature, Response22$Feature, Response23$Feature, Response24$Feature), AllWeights = cbind(Predweight01, Predweight11, Predweight12, Predweight21, Predweight22, Predweight23, Predweight24)))
}
