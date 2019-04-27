#Data
library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)


#Loading Models 
enetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\enet_solubility.rds')
marsModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\mars_solubility.rds')
svmRModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\svm_solubility.rds')
nnetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\nnet_solubility.rds')
rpartModel <- readRDS('rpart_solubility.rds')
ctreeModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\ctree_solubility.rds')
mtModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\M5_solubility.rds')
treebagModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\treebag_solubility.rds')
rfModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\rf_solubility.rds')
gbmModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\gbm_solubility.rds')
cbModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\cubist_solubility.rds')
knnModel <- readRDS('KNN_solubility.rds')


#Assessing Models ------------------------------------
#Resamples (remember quick models lmModel, plsModel, knnModel)
allResamples <- resamples(list('Linear Reg' = lmModel,
                               'PLS' = plsModel,
                               'Elastic Net' = enetModel,
                               'KNN' = knnModel,
                               'MARS' = marsModel,
                               'SVM' = svmRModel,
                               'Neural Networks' = nnetModel,
                               'CART' = rpartModel,
                               'Cond Inf Tree' = ctreeModel,
                               'Bagged Tree' = treebagModel,
                               'Boosted Tree' = gbmModel, 
                               'Random Forest' = rfModel,
                               "Cubist" = cbModel,
                               'ModelTrees' = mtModel))

#Parallel Coordinate Plots (p231)

parallelplot(allResamples, metric = 'RMSE')
parallelplot(allResamples, metric = 'Rsquared')

dotplot(allResamples, metric="RMSE")
summary(diff(allResamples))


#Residuals 
#KNN
knnModel 
plot(knnModel)
knnTraining_pred <- predict(knnModel, solTrainXtrans)
knnTraining_residuals <- knnTraining_pred - solTrainY
#predicted vs observed 
plot(knnTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(knnTraining_pred, knnTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')

#CART
rpartModel
plot(rpartModel)
rpartTraining_pred <- predict(rpartModel, solTrainXtrans)
rpartTraining_residuals <- rpartTraining_pred - solTrainY
#predicted vs observed 
plot(rpartTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(rpartTraining_pred, rpartTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')


#Con Inf Tree
ctreeModel
plot(ctreeModel)
ctreeTraining_pred <- predict(ctreeModel, solTrainXtrans)
ctreeTraining_residuals <- ctreeTraining_pred - solTrainY
#predicted vs observed 
plot(ctreeTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(ctreeTraining_pred, ctreeTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')


#treebagModel 
treebagModel
bagTraining_pred <- predict(treebagModel, solTrainXtrans)
bagTraining_residuals <- bagTraining_pred - solTrainY
#predicted vs observed 
plot(bagTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(bagTraining_pred, bagTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')

#nnet
nnetModel
plot(nnetModel)

nnetTraining_pred <- predict(nnetModel, solTrainXtrans)
nnetTraining_residuals <- nnetTraining_pred - solTrainY
#predicted vs observed 
plot(nnetTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(nnetTraining_pred, nnetTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')

#LM
lmModel
lmTraining_pred <- predict(lmModel, solTrainXtrans)
lmTraining_residuals <- lmTraining_pred - solTrainY
#predicted vs observed 
plot(lmTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(lmTraining_pred, lmTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')


#Mars 
marsModel
plot(marsModel)

marsTraining_pred <- predict(marsModel, solTrainXtrans)
marsTraining_residuals <- marsTraining_pred - solTrainY

#predicted vs observed 
plot(marsTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')


#Predicted vs residuals 
plot(marsTraining_pred, marsTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')

#PLS
plsModel
plot(plsModel)
plsTraining_pred <- predict(plsModel, solTrainXtrans)
plsTraining_residuals <- plsTraining_pred - solTrainY

#predicted vs observed 
dev.off()
plot(plsTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')

#Predicted vs residuals 
plot(plsTraining_pred, plsTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')

#enet 
enetModel
plot(enetModel)

enetTraining_pred <- predict(enetModel, solTrainXtrans)
enetTraining_residuals <- enetTraining_pred - solTrainY

#predicted vs observed 
plot(enetTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')

#Predicted vs residuals 
plot(enetTraining_pred, enetTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')


#mtModel 
mtModel
plot(mtModel)

#Rf 
rfModel
plot(rfModel)

#SVM
svmRModel
plot(svmRModel)

#BoostgbmModel
gbmModel 
plot(gbmModel)

#Cubist 
cbModel 
plot(cbModel)
cbTraining_pred <- predict(cbModel, solTrainXtrans)
cbTraining_residuals <- cbTraining_pred - solTrainY

#predicted vs observed 
plot(cbTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')

#Predicted vs residuals 
plot(cbTraining_pred, cbTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')
