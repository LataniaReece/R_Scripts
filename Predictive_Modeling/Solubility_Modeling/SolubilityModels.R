library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)


ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
linearReg <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'lm',
                   trControl = ctrl)
print('Linear Model Finished')

set.seed(669)
plsModel <- train(x = solTrainXtrans,
                  y = solTrainY,
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 15,
                  trControl = ctrl)

print('plsModel Finished')

enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1),
                        .fraction =seq(0.05, 1, length = 20))

set.seed(669)
enetModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'enet',
                   preProc = c('center', 'scale'),
                   tuneGrid = enetGrid,
                   trControl = ctrl)

saveRDS(enetModel, 'enet_solubility.rds')
print('enetModel Finished')

set.seed(669)
marsModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                    method = 'earth',
                    tuneGrid = expand.grid(.degree = 1,
                                           .nprune = 2:25),
                    trControl = ctrl)
saveRDS(marsModel, 'mars_solubility.rds')
print('Mars Model Finished')

set.seed(669)
svmRModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'svmRadial',
                   tuneLength = 15,
                   preProc = c('center', 'scale'),
                   trControl = ctrl)
saveRDS(svmRModel, 'svm_solubility.rds')
print('svmRModel Finished')

nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
                        .size = seq(1,27, by = 2),
                        .bag = FALSE)
set.seed(669)
nnetModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'avNNet',
                   tuneGrid = nnetGrid,
                   preProc = c('center', 'scale'),
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 1000,
                   trControl = ctrl)

saveRDS(nnetModel, 'nnet_solubility.rds')
print('nnetModel Finished')

set.seed(669)
rpartModel <- train(x = solTrainXtrans,
                    y = solTrainY,
                    method = 'rpart',
                    tuneLength = 30,
                    trControl = ctrl)

saveRDS(rpartModel, 'rpart_solubility.rds')
print('rpart model Finished')

set.seed(669)
ctreeModel <- train(x = solTrainXtrans,
                    y = solTrainY,
                    method = 'ctree',
                    tuneLength = 10,
                    trControl = ctrl)
saveRDS(ctreeModel, 'ctree_solubility.rds')
print('ctree model Finished')



set.seed(669)
treebagModel <- train(x = solTrainXtrans,
                      y = solTrainY,
                      method = 'treebag',
                      trControl = ctrl)
saveRDS(treebagModel, 'treebag_solubility.rds')
print('treebagModel model Finished')

set.seed(669)
rfModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'rf',
                 tuneLength =  10,
                 ntrees = 1000,
                 importance = TRUE,
                 trControl = ctrl)

saveRDS(rfModel, 'rf_solubility.rds')
print('rf model Finished')

gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                  n.minobsinnode = 10)
set.seed(669)
gbmModel <- train(x = solTrainXtrans,
                  y = solTrainY,
                  method = 'gbm',
                  tuneGrid = gbmGrid,
                  verbose = FALSE,
                  trControl = ctrl)

saveRDS(gbmModel, 'gbm_solubility.rds')
print('gbm model Finished')

cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModel <- train(x = solTrainXtrans,
                 y = solTrainY, 
                 method = 'cubist',
                 tuneGrid = cubistGrid,
                 trControl = ctrl)

saveRDS(cbModel, 'cubist_solubility.rds')
print('cbModel model Finished')

set.seed(669)
mtModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'M5',
                 trControl = ctrl)
saveRDS(mtModel, 'M5_solubility.rds')
print('M5 model Finished')

#Loading RDS Modelsc
choose.dir()
enetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\enet_solubility.rds')
marsModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\mars_solubility.rds')
svmRModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\svm_solubility.rds')
nnetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\nnet_solubility.rds')
rpartModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\rpart_solubility.rds')
ctreeModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\ctree_solubility.rds')
mtModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\M5_solubility.rds')
treebagModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\treebag_solubility.rds')
rfModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\rf_solubility.rds')
gbmModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\gbm_solubility.rds')
cbModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\Solubility_Modeling\\cubist_solubility.rds')

#Resamples
allResamples <- resamples(list('Linear Reg' = lmModel,
                               'PLS' = plsModel,
                               'Elastic Net' = enetModel,
                               'MARS' = marsModel,
                               'SVM' = svmRModel,
                               'Neural Networks' = nnetModel,
                               'CART' = rpartModel,
                               'Cond Inf Tree' = ctreeModel,
                               'Bagged Tree' = treebagModel,
                               'Boosted Tree' = gbmModel, 
                               'Random Forest' = rfModel,
                               "Cubist" = cbModel))
