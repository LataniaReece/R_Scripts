#Create a test set of the test set 

library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)

trainingX <- solTrainXtrans
trainingY <- solTrainY
testX <- solTestXtrans
testY <- solTestY
#Creating CV Set and Test Set
set.seed(669)
sample_rows <- sample(nrow(testX), size = (nrow(testX)/2), replace = FALSE)
CvX <- testX[sample_rows,]
CvY <- testY[sample_rows]
new_testX <- testX[-sample_rows,]
new_testY <- testY[-sample_rows]

#check
#sum(rownames(solCvXtrans) %in% rownames(solTestXtrans))

#Reloading Models 
ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
lmModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'lm',
                 trControl = ctrl)

set.seed(669)
plsModel <- train(x = solTrainXtrans,
                  y = solTrainY,
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 30,
                  trControl = ctrl)


enetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\enet_solubility.rds')
marsModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\mars_solubility.rds')
svmRModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\svm_solubility.rds')
nnetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\nnet_solubility.rds')
rpartModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\rpart_solubility.rds')
ctreeModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\ctree_solubility.rds')
mtModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\M5_solubility.rds')
treebagModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\treebag_solubility.rds')
rfModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\rf_solubility.rds')
gbmModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\gbm_solubility.rds')
cbModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\cubist_solubility.rds')
knnModel <- readRDS("C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\KNN_solubility.rds")

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

#Predictions with CV set - just using the top four models (gbm, SVM, Random Forests, Cubist)---------
#gbm
boosted_pred <- predict(gbmModel, trainingX)
boostedPR <- postResample(boosted_pred, trainingY)
rmses_training = c(boostedPR[1])
r2s_training = c(boostedPR[2])
methods = c("gbm")

boosted_pred <- predict(gbmModel, CvX)
boostedPR <- postResample(boosted_pred, CvY)
rmses_Cv = c(boostedPR[1])
r2s_Cv = c(boostedPR[2])

#SVM
svm_pred <- predict(svmRModel, trainingX)
svmPR <- postResample(svm_pred, trainingY)
rmses_training = c(rmses_training, svmPR[1])
r2s_training = c(r2s_training, svmPR[2])
methods = c(methods, "svm")

svm_pred <- predict(svmRModel, CvX)
svmPR <- postResample(svm_pred, CvY)
rmses_Cv = c(rmses_Cv, svmPR[1])
r2s_Cv = c(r2s_Cv, svmPR[2])

#Random Forests 
rf_pred <- predict(rfModel, trainingX)
rfPR <- postResample(rf_pred, trainingY)
rmses_training = c(rmses_training, rfPR[1])
r2s_training = c(r2s_training, rfPR[2])
methods = c(methods, "rf")

rf_pred <- predict(rfModel, CvX)
rfPR <- postResample(rf_pred, CvY)
rmses_Cv = c(rmses_Cv, rfPR[1])
r2s_Cv = c(r2s_Cv, rfPR[2])

#Cubist 
cubist_pred <- predict(cbModel, trainingX)
cubistPR <- postResample(cubist_pred, trainingY)
rmses_training = c(rmses_training, cubistPR[1])
r2s_training = c(r2s_training, cubistPR[2])
methods = c(methods, "cubist")

cubist_pred <- predict(cbModel, CvX)
cubistPR <- postResample(cubist_pred, CvY)
rmses_Cv = c(rmses_Cv, cubistPR[1])
r2s_Cv = c(r2s_Cv, cubistPR[2])

# Package the results up:---------------------
#Best models are at the bottom of the lists
#Training
res_training = data.frame( rmse=rmses_training, r2=r2s_training )
rownames(res_training) = methods
training_order = order( -res_training$rmse )
res_training = res_training[ training_order, ]

#CV
res_Cv = data.frame( rmse=rmses_Cv, r2=r2s_Cv )
rownames(res_Cv) = methods
training_order = order( -res_Cv$rmse )
res_Cv = res_Cv[ training_order, ] 

