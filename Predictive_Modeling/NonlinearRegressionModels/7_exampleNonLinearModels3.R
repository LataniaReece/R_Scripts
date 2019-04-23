library(AppliedPredictiveModeling)
data(permeability)
data <- data.frame(fingerprints, permeability = permeability)

#data partition
library(caret)
set.seed(122)
inTrain <- createDataPartition(data$permeability,
                               p = 0.7,
                               list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#Models---------------
#first remove sparse and unbalanced variables first 
preds <- training[,-1108]
train_KeepVar<- preds[,-nearZeroVar(preds)]
#now pca
pca_training <- preProcess(KeepVar, method = c('center', 'scale', 'pca'), thresh = 0.95)
new_training <- predict(pca_training, KeepVar)
new_training <- cbind(new_training, permeability = training$permeability)
head(new_training)

#testing: 
test_keepVars <- testing[,-nearZeroVar(preds)]
new_testing <- predict(pca_training, test_keepVars)

#variables that you should keep = KeepVar
#preprocess them - center, scale and pca
ctrl <- trainControl(method = 'repeatedcv',
                     number = 10, 
                     repeats = 3)

#MARS 
marsGrid <- expand.grid(.nprune = 2:50, .degree = 1)
set.seed(122)
marsModel <- train(permeability ~., 
                    data = new_training,
                    method = 'earth',
                    tuneGrid = marsGrid,
                    trControl = ctrl)

marsModel 
saveRDS(marsModel, 'marsModel_permeability.rds')
plot(marsModel)
varImp(marsModel)

#residuals
mars_pred <- predict(marsModel)
mars_res <- mars_pred - new_training$permeability
#predicted vs observed
xyplot(new_training$permeability ~ mars_pred,
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

plot(new_training$permeability ~ new_training$PC1)
#KNN
#knn model
k_grid <- expand.grid(k = 1:15)
set.seed(100)
knnModel <- train(permeability ~.,
                  data = new_training,
                  method = 'knn',
                  tuneGrid = k_grid,
                  trControl = ctrl)
knnModel
plot(knnModel)

#knn residuals
knn_pred <- predict(knnModel)
knn_res <- knn_pred - new_training$permeability
#predicted vs observed
xyplot(new_training$permeability ~ knn_pred,
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
svmLinear <- train(permeability ~., 
                   data = new_training, 
                   method = 'svmLinear',
                   preProc = c('center', 'scale'),
                   tuneGrid = grid,
                   trControl = ctrl)
svmLinear
plot(svmLinear)
saveRDS(svmLinear, "svmLinear_permeability.rds")
svmLinear <- readRDS('svmLinear_permeability.rds')
#linsvm residuals--------------------
lin_pred <- predict(svmLinear)
lin_res <- lin_pred - new_training$permeability
#predicted vs observed
xyplot(new_training$permeability ~ lin_pred,
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
#RadialSVM
grid_radial <- expand.grid(sigma = seq(0,1,0.1),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
set.seed(100)
svmRadial <- train(permeability ~., 
                   data = new_training, 
                   method = 'svmRadial',
                   preProc = c('center', 'scale'),
                   tuneGrid = grid_radial,
                   trControl = ctrl)
svmRadial
plot(svmRadial)
saveRDS(svmRadial, "svmRadial_permeability.rds")
svmRadial <- readRDS('svmRadial_permeability.rds')
#radialsvm residuals-------------------------------
rad_pred <- predict(svmRadial)
rad_res <- rad_pred - new_training$permeability
#predicted vs observed
xyplot(new_training$permeability ~ rad_pred,
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
nnetModel <- train(permeability ~.,
                   data = new_training,
                   method = 'avNNet',
                   preProcess = c('center', 'scale'),
                   linout = TRUE,
                   trControl = ctrl,
                   trace = FALSE)
saveRDS(nnetModel, "nnet_permeability.rds")
nnetModel <- readRDS('nnet_permeability.rds')
nnetModel

#nnet residuals-------------------------------
nnet_pred <- predict(nnetModel)
nnet_res <- nnet_pred - new_training$permeability
#predicted vs observed
xyplot(new_training$permeability
       ~ nnet_pred,
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

#KNN seems to be the best model in this case...
preds <- predict(knnModel, new_testing)
postResample(preds, new_testing$permeability)
