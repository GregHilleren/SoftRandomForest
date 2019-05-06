library(SoftRandomForest)
A = rnorm(1000, 200, 4)
B = rnorm(1000, 20, 6)
C = rnorm(1000, 175, 4)
D = rnorm(1000, 30, 2)
traindata = matrix(NA, 1000, 4)
traindata[1:333,] = cbind(B[1:333], D[1:333], A[1:333], C[1:333])
traindata[334:666,] = cbind(D[334:666], A[334:666], B[334:666], C[334:666])
traindata[667:1000,] = cbind(C[667:1000], B[667:1000], A[667:1000], D[667:1000])

seq = 1:1000
responsevec = c(rep("A", 333), rep("B", 333), rep("C", 334))
responsemat = SoftClassMatrix(responsevec)
index = sample(seq, 750, replace = FALSE)
testindex = sample(seq[-index], 250, replace = FALSE)
prediction = SoftClassForest(trainresponses = responsemat[index,], train = traindata[index,], test = traindata[testindex,], ntry = 2, ntrees = 50, depth = 3, bag = TRUE)
responsemattest = responsemat[testindex,]
actualresponse = numeric(250)
for(i in 1:250)
{
  actualresponse[i] = which.max(responsemattest[i,])
}

sum(prediction == actualresponse) == 250

Obs = sample(1:1000, 2, replace = FALSE)
ObsOut = SoftObservation(response = responsemat[,1], train = traindata, depth = 5, keep = TRUE, observation = Obs, export = FALSE)
sum(0 < ObsOut$Prediction & ObsOut$Prediction < 1) == 2
0 < min(ObsOut$AllWeights) & max(ObsOut$AllWeights) < 1

    