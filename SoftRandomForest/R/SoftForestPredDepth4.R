#' @export

SoftForestPredDepth4 = function(trainresponse, train, test, num.features, ntry, keep = FALSE)
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
  Response31 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0*Response11$weights0*Response21$weights0)
  Response32 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0*Response11$weights0*Response21$weights1)
  Response33 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0*Response11$weights1*Response22$weights0)
  Response34 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights0*Response11$weights1*Response22$weights1)
  Response35 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1*Response12$weights0*Response23$weights0)
  Response36 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1*Response12$weights0*Response23$weights1)
  Response37 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1*Response12$weights1*Response24$weights0)
  Response38 = BestForestSplit(trainresponse, train, num.features, ntry, weights = Response01$weights1*Response12$weights1*Response24$weights1)
  
  Predweight01 =  as.numeric(inv.logit(Response01$fit$coefficients[1] + Response01$fit$coefficients[2]*test[,Response01$Feature]))
  Predweight11 =  as.numeric(inv.logit(Response11$fit$coefficients[1] + Response11$fit$coefficients[2]*test[,Response11$Feature]))
  Predweight12 =  as.numeric(inv.logit(Response12$fit$coefficients[1] + Response12$fit$coefficients[2]*test[,Response12$Feature]))
  Predweight21 =  as.numeric(inv.logit(Response21$fit$coefficients[1] + Response21$fit$coefficients[2]*test[,Response21$Feature]))
  Predweight22 =  as.numeric(inv.logit(Response22$fit$coefficients[1] + Response22$fit$coefficients[2]*test[,Response22$Feature]))
  Predweight23 =  as.numeric(inv.logit(Response23$fit$coefficients[1] + Response23$fit$coefficients[2]*test[,Response23$Feature]))
  Predweight24 =  as.numeric(inv.logit(Response24$fit$coefficients[1] + Response24$fit$coefficients[2]*test[,Response24$Feature]))
  Predweight31 =  as.numeric(inv.logit(Response31$fit$coefficients[1] + Response31$fit$coefficients[2]*test[,Response31$Feature]))
  Predweight32 =  as.numeric(inv.logit(Response32$fit$coefficients[1] + Response32$fit$coefficients[2]*test[,Response32$Feature]))
  Predweight33 =  as.numeric(inv.logit(Response33$fit$coefficients[1] + Response33$fit$coefficients[2]*test[,Response33$Feature]))
  Predweight34 =  as.numeric(inv.logit(Response34$fit$coefficients[1] + Response34$fit$coefficients[2]*test[,Response34$Feature]))
  Predweight35 =  as.numeric(inv.logit(Response35$fit$coefficients[1] + Response35$fit$coefficients[2]*test[,Response35$Feature]))
  Predweight36 =  as.numeric(inv.logit(Response36$fit$coefficients[1] + Response36$fit$coefficients[2]*test[,Response36$Feature]))
  Predweight37 =  as.numeric(inv.logit(Response37$fit$coefficients[1] + Response37$fit$coefficients[2]*test[,Response37$Feature]))
  Predweight38 =  as.numeric(inv.logit(Response38$fit$coefficients[1] + Response38$fit$coefficients[2]*test[,Response38$Feature]))
  
  Prediction = (1-Predweight01)*(1-Predweight11)*(1-Predweight21)*Predweight31 + (1-Predweight01)*(1-Predweight11)*Predweight21*Predweight32 + (1-Predweight01)*Predweight11*(1-Predweight22)*Predweight33 + (1-Predweight01)*Predweight11*Predweight22*Predweight34 + Predweight01*(1-Predweight12)*(1-Predweight23)*Predweight35 + Predweight01*(1-Predweight12)*Predweight23*Predweight36 + Predweight01*Predweight12*(1-Predweight24)*Predweight37 + Predweight01*Predweight12*Predweight24*Predweight38 
  
  if(keep == FALSE) return(Prediction)
  if(keep == TRUE) return(list(Prediction = Prediction, AllFeatures = cbind(Response01$Feature, Response11$Feature, Response12$Feature, Response21$Feature, Response22$Feature, Response23$Feature, Response24$Feature, Response31$Feature, Response32$Feature, Response33$Feature, Response34$Feature, Response35$Feature, Response36$Feature, Response37$Feature, Response38$Feature), AllWeights = cbind(Predweight01, Predweight11, Predweight12, Predweight21, Predweight22, Predweight23, Predweight24, Predweight31, Predweight32, Predweight33, Predweight34, Predweight35, Predweight36, Predweight37, Predweight38)))
}
