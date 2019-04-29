library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)

#Models----------------------------------------------------
ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
lmModelNT <- train(x = solTrainX,
                 y = solTrainY,
                 method = 'lm',
                 trControl = ctrl)
print('Linear Model Finished')

set.seed(669)
plsModelNT <- train(x = solTrainX,
                  y = solTrainY,
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 30,
                  trControl = ctrl)

print('plsModel Finished')

enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1),
                        .fraction =seq(0.05, 1, length = 20))

set.seed(669)
enetModelNT <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'enet',
                   preProc = c('center', 'scale'),
                   tuneGrid = enetGrid,
                   trControl = ctrl)

saveRDS(enetModelNT, 'enet_noTransSolubility.rds')
print('enetModel Finished')


knnGrid <- expand.grid(k = 1:30)
set.seed(669)

knnModelNT = train(x = solTrainX,
                   y = solTrainY,
                   method="knn",
                   preProc= c('center', 'scale'),
                   tuneGrid = knnGrid,
                   trControl = ctrl)

saveRDS(knnModelNT, 'KNN_noTransSolubility.rds')
print( 'KNN Model Finished')

set.seed(669)
marsModelNT <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'earth',
                   tuneGrid = expand.grid(.degree = 1,
                                          .nprune = 2:25),
                   trControl = ctrl)
saveRDS(marsModelNT, 'mars_noTransSolubility.rds')
print('Mars Model Finished')

set.seed(669)
svmRModelNT <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'svmRadial',
                   tuneLength = 15,
                   preProc = c('center', 'scale'),
                   trControl = ctrl)
saveRDS(svmRModelNT, 'svm_noTransSolubility.rds')
print('svmRModel Finished')

nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
                        .size = seq(1,27, by = 2),
                        .bag = FALSE)
set.seed(669)
nnetModelNT <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'avNNet',
                   tuneGrid = nnetGrid,
                   preProc = c('center', 'scale'),
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 1000,
                   trControl = ctrl)

saveRDS(nnetModelNT, 'nnet_noTransSolubility.rds')
print('nnetModel Finished')

set.seed(669)
rpartModelNT <- train(x = solTrainX,
                    y = solTrainY,
                    method = 'rpart',
                    tuneLength = 30,
                    trControl = ctrl)

saveRDS(rpartModelNT, 'rpart_noTransSolubility.rds')
print('rpart model Finished')

set.seed(669)
ctreeModelNT <- train(x = solTrainX,
                    y = solTrainY,
                    method = 'ctree',
                    tuneLength = 10,
                    trControl = ctrl)
saveRDS(ctreeModelNT, 'ctree_noTransSolubility.rds')
print('ctree model Finished')



set.seed(669)
treebagModelNT <- train(x = solTrainX,
                      y = solTrainY,
                      method = 'treebag',
                      trControl = ctrl)
saveRDS(treebagModelNT, 'treebag_noTransSolubility.rds')
print('treebagModel model Finished')

set.seed(669)
rfModelNT <- train(x = solTrainX,
                 y = solTrainY,
                 method = 'rf',
                 tuneLength =  10,
                 ntrees = 1000,
                 importance = TRUE,
                 trControl = ctrl)

saveRDS(rfModelNT, 'rf_noTransSolubility.rds')
print('rf model Finished')

gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10)
set.seed(669)
gbmModelNT <- train(x = solTrainX,
                  y = solTrainY,
                  method = 'gbm',
                  tuneGrid = gbmGrid,
                  verbose = FALSE,
                  trControl = ctrl)

saveRDS(gbmModelNT, 'gbm_noTransSolubility.rds')
print('gbm model Finished')

cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModelNT <- train(x = solTrainX,
                 y = solTrainY, 
                 method = 'cubist',
                 tuneGrid = cubistGrid,
                 trControl = ctrl)

saveRDS(cbModelNT, 'cubist_noTransSolubility.rds')
print('cbModel model Finished')

set.seed(669)
mtModelNT <- train(x = solTrainX,
                 y = solTrainY,
                 method = 'M5',
                 trControl = ctrl)
saveRDS(mtModelNT, 'M5_noTransSolubility.rds')
print('M5 model Finished')

file.choose()

enetModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\enet_noTransSolubility.rds')
marsModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\mars_noTransSolubility.rds')
svmRModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\svm_noTransSolubility.rds')
nnetModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\nnet_noTransSolubility.rds')
rpartModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\rpart_noTransSolubility.rds')
ctreeModelNT <- readRDS("C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\ctree_noTransSolubility.rds")
mtModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\M5_noTransSolubility.rds')
treebagModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\treebag_noTransSolubility.rds')
rfModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\rf_noTransSolubility.rds')
gbmModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\gbm_noTransSolubility.rds')
cbModelNT <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\cubist_noTransSolubility.rds')
knnModelNT <- readRDS("C:\\Users\\reece\\Desktop\\R_Scripts\\Models_rds\\KNN_noTransSolubility.rds")



#Assessing Models ------------------------------------
#Not Transformed Predictors
allResamplesNT <- resamples(list('Linear Reg' = lmModelNT,
                               'PLS' = plsModelNT,
                               'Elastic Net' = enetModelNT,
                               'KNN' = knnModelNT,
                               'MARS' = marsModelNT,
                               'SVM' = svmRModelNT,
                               'Neural Networks' = nnetModelNT,
                               'CART' = rpartModelNT,
                               'Cond Inf Tree' = ctreeModelNT,
                               'Bagged Tree' = treebagModelNT,
                               'Boosted Tree' = gbmModelNT, 
                               'Random Forest' = rfModelNT,
                               "Cubist" = cbModelNT,
                               'Model Trees' = mtModelNT))

#Parallel Coordinate Plots (p231)

parallelplot(allResamplesNT, metric = 'RMSE')
parallelplot(allResamplesNT, metric = 'Rsquared')

dotplot(allResamplesNT, metric="RMSE")
summary(diff(allResamplesNT))

#Transformed Predictors 

ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
lmModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'lm',
                 trControl = ctrl)
# print('Linear Model Finished')

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
                              'Model Trees' = mtModel))

#Parallel Coordinate Plots (p231)

parallelplot(allResamples, metric = 'RMSE')
parallelplot(allResamples, metric = 'Rsquared')

dotplot(allResamples, metric="RMSE")
summary(diff(allResamples))


allResamplesBoth <-resamples(list('Linear Reg' = lmModel,
                                  'Linear Reg NT' = lmModelNT,
                                  'PLS' = plsModel,
                                  'PLS NT' = plsModelNT,
                                  'Elastic Net' = enetModel,
                                  'Elastic Net NT' = enetModelNT,
                                  'KNN' = knnModel,
                                  'KNN NT' = knnModelNT,
                                  'MARS' = marsModel,
                                  'MARS NT' = marsModelNT,
                                  'SVM' = svmRModel,
                                  'SVM NT' = svmRModelNT,
                                  'Neural Networks' = nnetModel,
                                  'Neural Networks NT' = nnetModelNT,
                                  'CART' = rpartModel,
                                  'CART NT' = rpartModelNT,
                                  'Cond Inf Tree' = ctreeModel,
                                  'Cond Inf Tree NT' = ctreeModelNT,
                                  'Bagged Tree' = treebagModel,
                                  'Bagged Tree NT' = treebagModelNT,
                                  'Boosted Tree' = gbmModel, 
                                  'Boosted Tree NT' = gbmModelNT, 
                                  'Random Forest' = rfModel,
                                  'Random Forest NT' = rfModelNT,
                                  "Cubist" = cbModel,
                                  "Cubist NT" = cbModelNT,
                                  'Model Trees' = mtModel, 
                                  'Model Trees NT' = mtModelNT))

parallelplot(allResamplesBoth, metric = 'RMSE')
parallelplot(allResamplesBoth, metric = 'Rsquared')

dotplot(allResamplesBoth, metric="RMSE")
summary(diff(allResamplesBoth))
summary(allResamplesBoth)


