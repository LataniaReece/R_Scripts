library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)

#Models----------------------------------------------------
ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
lmModel <- train(x = solTrainX,
                 y = solTrainY,
                 method = 'lm',
                 trControl = ctrl)
print('Linear Model Finished')

set.seed(669)
plsModel <- train(x = solTrainX,
                  y = solTrainY,
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 15,
                  trControl = ctrl)

print('plsModel Finished')

enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1),
                        .fraction =seq(0.05, 1, length = 20))

set.seed(669)
enetModel <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'enet',
                   preProc = c('center', 'scale'),
                   tuneGrid = enetGrid,
                   trControl = ctrl)

saveRDS(enetModel, 'enet_noTransSolubility.rds')
print('enetModel Finished')

set.seed(669)
knnModel = train(x = solTrainX, 
                 y = solTrainY,
                 method="knn",
                 preProc= c('center', 'scale'),
                 tuneLength=10,
                 trControl = ctrl)

saveRDS(knnModel, 'KNN_noTransSolubility.rds')
print( 'KNN Model Finished')

set.seed(669)
marsModel <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'earth',
                   tuneGrid = expand.grid(.degree = 1,
                                          .nprune = 2:25),
                   trControl = ctrl)
saveRDS(marsModel, 'mars_noTransSolubility.rds')
print('Mars Model Finished')

set.seed(669)
svmRModel <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'svmRadial',
                   tuneLength = 15,
                   preProc = c('center', 'scale'),
                   trControl = ctrl)
saveRDS(svmRModel, 'svm_noTransSolubility.rds')
print('svmRModel Finished')

nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
                        .size = seq(1,27, by = 2),
                        .bag = FALSE)
set.seed(669)
nnetModel <- train(x = solTrainX,
                   y = solTrainY,
                   method = 'avNNet',
                   tuneGrid = nnetGrid,
                   preProc = c('center', 'scale'),
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 1000,
                   trControl = ctrl)

saveRDS(nnetModel, 'nnet_noTransSolubility.rds')
print('nnetModel Finished')

set.seed(669)
rpartModel <- train(x = solTrainX,
                    y = solTrainY,
                    method = 'rpart',
                    tuneLength = 30,
                    trControl = ctrl)

saveRDS(rpartModel, 'rpart_noTransSolubility.rds')
print('rpart model Finished')

set.seed(669)
ctreeModel <- train(x = solTrainX,
                    y = solTrainY,
                    method = 'ctree',
                    tuneLength = 10,
                    trControl = ctrl)
saveRDS(ctreeModel, 'ctree_noTransSolubility.rds')
print('ctree model Finished')



set.seed(669)
treebagModel <- train(x = solTrainX,
                      y = solTrainY,
                      method = 'treebag',
                      trControl = ctrl)
saveRDS(treebagModel, 'treebag_noTransSolubility.rds')
print('treebagModel model Finished')

set.seed(669)
rfModel <- train(x = solTrainX,
                 y = solTrainY,
                 method = 'rf',
                 tuneLength =  10,
                 ntrees = 1000,
                 importance = TRUE,
                 trControl = ctrl)

saveRDS(rfModel, 'rf_noTransSolubility.rds')
print('rf model Finished')

gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10)
set.seed(669)
gbmModel <- train(x = solTrainX,
                  y = solTrainY,
                  method = 'gbm',
                  tuneGrid = gbmGrid,
                  verbose = FALSE,
                  trControl = ctrl)

saveRDS(gbmModel, 'gbm_noTransSolubility.rds')
print('gbm model Finished')

cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModel <- train(x = solTrainX,
                 y = solTrainY, 
                 method = 'cubist',
                 tuneGrid = cubistGrid,
                 trControl = ctrl)

saveRDS(cbModel, 'cubist_noTransSolubility.rds')
print('cbModel model Finished')

set.seed(669)
mtModel <- train(x = solTrainX,
                 y = solTrainY,
                 method = 'M5',
                 trControl = ctrl)
saveRDS(mtModel, 'M5_noTransSolubility.rds')
print('M5 model Finished')
