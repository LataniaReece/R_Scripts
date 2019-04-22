library(mlbench)
set.seed(200)

#using mlbench.friedman1 to similate nonlinear data 
trainingData <- mlbench.friedman1(200, sd = 1)
trainingData$x <- data.frame(trainingData$x)
#look at the data 
library(caret)
featurePlot(trainingData$x, trainingData$y)

testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)

#Models---------------
#KNN
set.seed(100)
knnModel <- train(x = trainingData$x, 
                  y = trainingData$y,
                  method = 'knn', 
                  preProcess = c('center', 'scale'),
                  tuneLength = 10)
knnModel
knnPred <- predict(knnModel, newdata = testData$x)
postResample(knnPred, testData$y)


#residuals?
knn_pred <- predict(knnModel)
knn_resid <- knn_pred - trainingData$y
#predicted vs observed
xyplot(trainingData$y ~ knn_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(knn_resid ~ knn_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#Mars 
set.seed(100)
marsModel <- train(x = trainingData$x,
                   y = trainingData$y,
                   method = 'earth',
                   tuneLength = 10)
marsModel
marsPred <- predict(marsModel, newdata = testData$x)
postResample(marsPred, testData$y)

#residuals?
mars_pred <- predict(marsModel)
mars_resid <- mars_pred - trainingData$y
#predicted vs observed
xyplot(trainingData$y ~ mars_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(mars_resid ~ mars_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })
varImp(marsModel)

#SVM
#linear
set.seed(100)
svmLinear <- train(y = trainingData$y,
                   x = trainingData$x, 
                   method = 'svmLinear',
                   preProc = c('center', 'scale'),
                   tuneLength = 20)
svmLinear
#predictions 
lsvmPred <- predict(svmLinear, testData$x)
postResample(lsvmPred, testData$y)

#residuals
lin_pred <- predict(svmLinear)
lin_res <- lin_pred - trainingData$y
#predicted vs observed
xyplot(trainingData$y ~ lin_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(lin_res ~ lin_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#radial
set.seed(100)
svmRadial <- train(y = trainingData$y,
                   x = trainingData$x, 
                   method = 'svmRadial',
                   preProc = c('center', 'scale'),
                   tuneLength = 20)
svmRadial
#predictions 
rsvmPred <- predict(svmRadial, testData$x)
postResample(rsvmPred, testData$y)

#residuals 
rad_pred <- predict(svmRadial)
rad_res <- rad_pred - trainingData$y
#predicted vs observed
xyplot(trainingData$y ~ rad_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(rad_res ~ rad_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#model comparison 
models = list(KNN = knnModel, MARS = marsModel, LinearSVM = svmLinear, RadialSVM = svmRadial)
resamps <- resamples(models, metric = 'RMSE')
summary(resamps)

?resamples
