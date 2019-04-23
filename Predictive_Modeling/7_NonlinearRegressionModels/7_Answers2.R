#7.2. Ans

library(caret) 
library(mlbench)

set.seed(200)
trainingData = mlbench.friedman1(200,sd=1)
## We convert the 'x' data from a matrix to a data frame
## One reason we do this is that this will give the columns names
trainingData$x = data.frame(trainingData$x)

## Look at the data using feature plot or other methods
featurePlot(trainingData$x, trainingData$y)

#create test data
testData = mlbench.friedman1(5000,sd=1)
testData$x = data.frame(testData$x)

# Note we use the default "trainControl" bootstrap evaluations for each of the models below: 
# A K-NN model:
# 
set.seed(0)
knnModel = train(x=trainingData$x, y=trainingData$y,
                 method="knn",
                 preProc=c("center","scale"),
                 tuneLength=10)

knnPred = predict(knnModel, newdata=testData$x)
## The function 'postResample' can be used to get the test set performance values
knnPR = postResample(pred=knnPred, obs=testData$y)
rmses = c(knnPR[1])
r2s = c(knnPR[2])
methods = c("KNN")

# A Neural Network model:
#
nnGrid = expand.grid( .decay=c(0,0.01,0.1), .size=1:10, .bag=FALSE )
set.seed(0)
nnetModel = train(x=trainingData$x, 
                  y=trainingData$y, 
                  method="nnet",
                  preProc=c("center", "scale"),
                  linout=TRUE,
                  trace=FALSE,
                  MaxNWts=10 * (ncol(trainingData$x)+1) + 10 + 1,
                  maxit=500)

nnetPred = predict(nnetModel, newdata=testData$x)
nnetPR = postResample(pred=nnetPred, obs=testData$y)
rmses = c(rmses,nnetPR[1])
r2s = c(r2s,nnetPR[2])
methods = c(methods,"NN")

# Averaged Neural Network models:
#
set.seed(0)
avNNetModel = train(x=trainingData$x, 
                    y=trainingData$y, 
                    method="avNNet",
                    preProc=c("center", "scale"),
                    linout=TRUE,trace=FALSE,
                    MaxNWts=10 * (ncol(trainingData$x)+1) + 10 + 1, maxit=500)

avNNetPred = predict(avNNetModel, newdata=testData$x)
avNNetPR = postResample(pred=avNNetPred, obs=testData$y)
rmses = c(rmses,avNNetPR[1])
r2s = c(r2s,avNNetPR[2])
methods = c(methods,"AvgNN")

# MARS model:
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
set.seed(0)
marsModel = train(x=trainingData$x, y=trainingData$y,
                  method="earth",
                  preProc=c("center", "scale"), 
                  tuneGrid=marsGrid)

marsPred = predict(marsModel, newdata=testData$x)
marsPR = postResample(pred=marsPred, obs=testData$y)
rmses = c(rmses,marsPR[1])
r2s = c(r2s,marsPR[2])
methods = c(methods,"MARS")

# Lets see what variables are most important: 
varImp(marsModel)

# A Support Vector Machine (SVM):
#
set.seed(0)
svmRModel = train(x=trainingData$x, y=trainingData$y, 
                  method="svmRadial", 
                  preProc=c("center", "scale"), 
                  tuneLength=20)

svmRPred = predict(svmRModel, newdata=testData$x)
svmPR = postResample(pred=svmRPred, obs=testData$y) 
rmses = c(rmses,svmPR[1])
r2s = c(r2s,svmPR[2])
methods = c(methods,"SVM")

res = data.frame( rmse=rmses, r2=r2s )
rownames(res) = methods

# Order the dataframe so that the best results are at the bottom:
#
res = res[ order( -res$rmse ), ]
print( "Final Results" ) 
print( res )
