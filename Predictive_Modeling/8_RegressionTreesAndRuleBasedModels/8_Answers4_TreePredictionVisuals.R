#8.4 ans

library(caret)
library(AppliedPredictiveModeling)
library(rpart)
library(randomForest)
library(Cubist)

set.seed(0)

data(solubility)

# We have the following variables in this dataset:
# 
## solTrainX: training set predictors in their natural units.
#
## solTrainXtrans: training set predictors after transformations for
##           skewness and centering/scaling.
#
## solTrainY: a vector of log10 solubility values for the training set.
#
## solTestX: test set predictors in their natural units.
#
## solTestXtrans: test set predictors after the same transformations used
##           on the training set are applied.
#
## solTestY: a vector of log10 solubility values for the training set.
#
# Make sure we don't access the unscaled variables by accident (we want to use the scaled variables): 
rm(solTrainX)
rm(solTestX)

# Use solTrainXtrans$MolWeight as our scalar predictor:
trainData = data.frame( x=solTrainXtrans$MolWeight, y=solTrainY )

# Plot the predictor data vs. the solubility:
#
plot( trainData$x, trainData$y, xlab='MolWeight', ylab='log10(solubility)' )

# For a sanity check fit a linear model and look at the predictions it gives: 
#
lmModel = lm( y ~ ., data=trainData ) 
lm_yhat = predict( lmModel, newdata=data.frame(x=solTestXtrans$MolWeight) )
plot( solTestY, lm_yhat ) 


# Part (a): (fit a simple regression tree):
#
# defaults: for rpart.control are cp=0.01, maxdepth=30
rPartModel = rpart( y ~ ., data=trainData, method="anova", control=rpart.control(cp=0.01,maxdepth=30) ) # decreasing cp makes deeper trees; increasing maxdepth
###plotcp(rPartModel)

# Plot the regression tree:
# 
plot(rPartModel); text(rPartModel)

# predict solubility with this regression tree: 
rPart_yHat = predict(rPartModel,newdata=data.frame(x=solTestXtrans$MolWeight))

plot( solTestXtrans$MolWeight, rPart_yHat, col='red', xlab='MolWeight', ylab='log10(solubility)', main='rpart test set predictions' )
lines( solTestXtrans$MolWeight, solTestY, type='p' )


# Part (b): (fit a randomforest):
#
rfModel = randomForest( y ~ ., data=trainData, ntree=500 ) # ntree=500, mtry=does not matter when we have a scalar feature 

# predict solubility:
rf_yHat = predict(rfModel,newdata=data.frame(x=solTestXtrans$MolWeight))

plot( solTestXtrans$MolWeight, rf_yHat, col='red', xlab='MolWeight', ylab='log10(solubility)', main='randomForest test set predictions' )
lines( solTestXtrans$MolWeight, solTestY, type='p' )


# Part (c): (fit different Cubist models):
#
cubistModel = cubist( data.frame( x=solTrainXtrans$MolWeight ), solTrainY, committees=1 ) # committees=1

# predict solubility:
cubist_yHat = predict(cubistModel,newdata=data.frame(x=solTestXtrans$MolWeight))

plot( solTestXtrans$MolWeight, cubist_yHat, col='red', xlab='MolWeight', ylab='log10(solubility)', main='cubist test set predictions' )
lines( solTestXtrans$MolWeight, solTestY, type='p' )


#increasing the cp = increasing what is considered important thus this increases SSE since less splits
#will be considered important even though they may be 

#increasing the number of trees in random forest didn't seem to have much of an effect once I got to about 2000 trees,
#kept predicting the same thing

#increaisng the comittess didn't really have much of an effect on the cubist model,
#kept predicting the same thing

