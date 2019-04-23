#p128

library(AppliedPredictiveModeling)
data(solubility)
#the data objects begin with 'Sol': 
ls(patter = 'solT')
#Note: they have already Box-Cox both test and train - trans objects

#Ordinary Least Regression----------------
#predictors and outcome need to be in same df
trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY

#lr model
set.seed(2)
lmFitAllPredictors <- lm(Solubility ~., data = trainingData)
summary(lmFitAllPredictors)

#predict
lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
head(lmPred1)

#combine observed and predicted in dataframe and estimate test set performance 
lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)
library(caret)
defaultSummary(lmValues1)

#robust linear model function - default uses huber instead of SSE, less sensitve to outliers 
library(MASS)
rlmFitAllPredictors <- rlm(Solubility ~., data = trainingData)

#using caret to trian 
ctrl <- trainControl(method = 'cv', number = 10)
set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY, 
                method = 'lm', trControl = ctrl)
lmFit1

#residual plots 
#predicted vs observed
xyplot(solTrainY ~ predict(lmFit1),
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })

#predicted vs residuals 
xyplot(resid(lmFit1) ~ predict(lmFit1),
       tupe = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#removing highly correlated predictors 
corThresh <- 0.9

tooHigh <- findCorrelation(cor(solTrainXtrans), corThresh)
corrPred <- names(solTrainXtrans)[tooHigh]
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered <- solTestXtrans[, -tooHigh]

#train with lm
set.seed(100)
lmFiltered <- train(solTrainXtrans, solTrainY, method = 'lm',
                    trControl  = ctrl)
lmFiltered

#train with rlm - predictor matrix cannot be singular - PCA 
set.seed(100)
rlmPCA <- train(solTrainXtrans, solTrainY,
                method = 'rlm',
                preProcess = 'pca',
                trControl = ctrl)
rlmPCA

#Partial Least Squares--------
library(pls)
plsFit <- plsr(Solubility ~., data = trainingData)
predict(plsFit, solTestXtrans[1:5,], ncomp = 1:2)


set.seed(100)
plsTune <- train(solTrainXtrans, solTrainY,
                 method = 'pls',
                 tuneLength = 20,
                 trControl = ctrl,
                 preProc = c('center', 'scale'))
plsTune

#Penalized Regression Models--------p134-----
#ridge regression
library(elasticnet) -
#using enet, predictor data must be in matrix 
#to specify ridge regression in elastic net use lamdba = 0.001
ridgeModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY,
                   lambda = 0.001)
#to produce ridge regression solution we define s =1  and mode = fraction
ridgePred <- predict(ridgeModel, newx = as.matrix(solTestXtrans),
                     s =1, mode = 'fraction',
                     type = 'fit')
head(ridgePred$fit)

#using caret train function - ridge regression
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 15))
set.seed(100)
ridgeRegFit <- train(solTrainXtrans, solTrainY,
                     method = 'ridge',
                     tuneGrid = ridgeGrid,
                     trControl = ctrl,
                     preProc = c('center', 'scale'))
ridgeRegFit

#lasso regression - lambda = 0.01, scale the predictors via NORMALIZE = true (p136)
#fraction of 1 = full solution wiht no penalty - coefficients are at their largest 
#smaller fraction values = more penalty in the model 
enetModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY,
                  lambda = 0.01, normalize = TRUE)
#prediction
enetPred <- predict(enetModel, newx = as.matrix(solTestXtrans),
                    s = .1, mode = 'fraction',
                    type = 'fit')
head(enetPred$fit)
#to determine which predictors are used in the model, 
#the predict method is used with type = 'coefficients'
enetPred <- predict(enetModel, newx = as.matrix(solTestXtrans),
                    s = .1, mode = 'fraction',
                    type = 'coefficients')
enetPred$coefficients

#using caret function - lasso regression
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(solTrainXtrans, solTrainY,
                  method = 'enet',
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
enetTune
plot(enetTune) #interpretation on p127-128 , 0 = lasso, 

