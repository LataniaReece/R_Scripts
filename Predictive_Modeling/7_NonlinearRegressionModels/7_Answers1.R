#7.1 Ans
dev.off()
library(caret) 
library(kernlab)

# Generate data to predict: 
set.seed(100)
x = runif(100,min=2,max=10)
y = sin(x) + rnorm(length(x)) * 0.25
sinData = data.frame(x=x,y=y)

plot(x,y)

## Create a grid of x values to use for prediction
dataGrid = data.frame(x=seq(2,100,length=100))

# Train some SVM models on this data and plot them:
#

# Reasonable looking predictions:
# 
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar="automatic", C=1, epsilon=0.1 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="blue" )

# A large value for sigma: 
# 
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar=list(sigma=100.0), C=1, epsilon=0.1 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="red" )
  
# A small value for sigma:
#
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar=list(sigma=1e-2), C=1, epsilon=0.1 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="green" )
  
# A very large value of the cost C (should overfit):
# 
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar="automatic", C=10^7, epsilon=0.1 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="red" )
  
# A very small value of the cost C (should underfit):
#
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar="automatic", C=10^(-2), epsilon=0.1 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="green", main='changes in cost' )
  
# A large value for epsilon: 
# 
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar="automatic", C=1, epsilon=0.5 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="red" )
  
# A small value for epsilon:
#
rbfSVM = ksvm( x=x, y=y, data=sinData, kernel="rbfdot", kpar="automatic", C=1, epsilon=0.001 )
modelPrediction = predict( rbfSVM, newdata=dataGrid )
points( x=dataGrid$x, y=modelPrediction[,1], type="l", col="green" )