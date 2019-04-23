library(caret)
data(tecator)

#Data partition 
data <- data.frame(absorp, fat = endpoints[,2])
set.seed(100)
inTrain <- createDataPartition(data$fat,
                               p= 0.7, 
                               list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#Models---------------
ctrl <- trainControl(method = 'repeatedcv',
                     number = 10, 
                     repeats = 3)

#MARS 
marsGrid <- expand.grid(.nprune = 2:50, .degree = 1)
set.seed(100)
marsModel <- train( fat ~., 
                    data = training,
                    method = 'earth',
                    tuneGrid = marsGrid,
                    trControl = ctrl)

marsModel 
plot(marsModel)
varImp(marsModel)
saveRDS(marsModel, 'MarsModel_tecator.rds')

#residuals
mars_pred <- predict(marsModel)
mars_res <- mars_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ mars_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(mars_res ~ mars_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })


#KNN
#pca first
pca_training <- preProcess(training[,-101], method = c('center', 'scale', 'pca'), thresh = 0.95)
knn_training <- predict(pca_training, training[,-101])
knn_training <- cbind(knn_training, fat = training$fat)
head(knn_training)

k_grid <- expand.grid(k = 1:15)
set.seed(100)
knnModel <- train(fat ~.,
                  data = knn_training,
                  method = 'knn',
                  tuneGrid = k_grid,
                  trControl = ctrl)
knnModel
plot(knnModel)

#knn residuals
knn_pred <- predict(knnModel)
knn_res <- knn_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ knn_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(knn_res ~ knn_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#SVMLinear 
set.seed(100)
grid <- expand.grid(C = c(2 %o% 10^(-3:3)))
svmLinear <- train(fat ~., 
                   data = training, 
                   method = 'svmLinear',
                   preProc = c('center', 'scale'),
                   tuneGrid = grid,
                   trControl = ctrl)
svmLinear
saveRDS(svmLinear, "svmLinear_tecator.rds")

#linsvm residuals--------------------
lin_pred <- predict(svmLinear)
lin_res <- lin_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ lin_pred,
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

plot(svmLinear)

#RadialSVM
grid_radial <- expand.grid(sigma = seq(0,1,0.1),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
set.seed(100)
svmRadial <- train(fat ~., 
                   data = training, 
                   method = 'svmRadial',
                   preProc = c('center', 'scale'),
                   tuneGrid = grid_radial,
                   trControl = ctrl)
svmRadial
plot(svmRadial)
saveRDS(svmRadial, "svmRadial_tecator.rds")

#radialsvm residuals-------------------------------
rad_pred <- predict(svmRadial)
rad_res <- rad_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ rad_pred,
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

#Neural Networks 

#nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
 #                       .size = c(1:10),
  #                      .bag = FALSE)
set.seed(100)
nnetModel <- train(fat ~.,
                   data = training,
                   method = 'avNNet',
                   preProcess = c('center', 'scale'),
                   linout = TRUE,
                   trControl = ctrl,
                   trace = FALSE)
saveRDS(nnetModel, "nnet_tecator.rds")
nnetModel

#nnet residuals-------------------------------
nnet_pred <- predict(nnetModel)
nnet_res <- nnet_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ nnet_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(nnet_res ~ nnet_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#comparing models 
models <- list(MARS = marsModel, 
               LinearSVM = svmLinear,
               RadialSVM = svmRadial,
               NeuralNetwork = nnetModel,
               KNN = knnModel)
resamps <- resamples(models)
summary(resamps)
