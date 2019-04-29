library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)


ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)


# nnetGrid2 <- expand.grid(.decay = c(0.001, 0.01, .1),
#                         .size = c(1:6),
#                         .bag = FALSE)
# 
# set.seed(669)
# nnetModel2 <- train(x = solTrainXtrans,
#                    y = solTrainY,
#                    method = 'avNNet',
#                    tuneGrid = nnetGrid2,
#                    preProc = c('center', 'scale'),
#                    linout = TRUE,
#                    trace = FALSE,
#                    maxit = 1000,
#                    trControl = ctrl)
# 
# saveRDS(nnetModel2, 'nnet2_solubility.rds')
# print('nnetModel2 Finished')
# 
# 
# #------------------------------------------------------------
# nnetGrid3 <- expand.grid(.decay = c(0.008, 0.01, 0.02, 0.03),
#                          .size = c(1:6),
#                          .bag = FALSE)
# 
# set.seed(669)
# nnetModel3 <- train(x = solTrainXtrans,
#                     y = solTrainY,
#                     method = 'avNNet',
#                     tuneGrid = nnetGrid3,
#                     preProc = c('center', 'scale'),
#                     linout = TRUE,
#                     trace = FALSE,
#                     maxit = 1000,
#                     trControl = ctrl)
# 
# saveRDS(nnetModel3, 'nnet3_solubility.rds')
# print('nnetModel3 Finished')
# #--------------------------------------------------------
# #does increasing nprune eventually lead to overfitting?
# set.seed(669)
# marsModel2 <- train(x = solTrainXtrans,
#                    y = solTrainY,
#                    method = 'earth',
#                    tuneGrid = expand.grid(.degree = 1:2,
#                                           .nprune = 2:50),
#                    trControl = ctrl)
# saveRDS(marsModel2, 'mars2_solubility.rds')
# print('Mars2 Model Finished')

#----------------------------------------------------------
#mtry around 127
# set.seed(669)
# rfModel2 <- train(x = solTrainXtrans,
#                  y = solTrainY,
#                  method = 'rf',
#                  tuneGrid =  expand.grid(mtry = c(100:155)),
#                  ntrees = 1000,
#                  importance = TRUE,
#                  trControl = ctrl)
# 
# saveRDS(rfModel2, 'rf2_solubility.rds')
# print('rf2 model Finished')

#----------------------------------------------------------

svmGrid2 <- expand.grid(sigma = c(0.01, .1, 1), 
                       C = c(100:200))
set.seed(669)
svmRModel2 <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'svmRadial',
                   tuneGrid = svmGrid2,
                   preProc = c('center', 'scale'),
                   trControl = ctrl)
saveRDS(svmRModel2, 'svm2_solubility.rds')
print('svmRModel2 Finished')


#Using Boostrap----------------------------------------------------------
print('Starting Bootstrap Samples')
enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1),
                        .fraction =seq(0.05, 1, length = 20))

set.seed(669)
enetModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'enet',
                   preProc = c('center', 'scale'),
                   tuneGrid = enetGrid)


saveRDS(enetModel, 'enet_solubility_boot.rds')
print('enetModel Finished')


knnGrid <- expand.grid(k = 1:30)
set.seed(669)

knnModel = train(x = solTrainXtrans,
                 y = solTrainY,
                 method="knn",
                 preProc= c('center', 'scale'),
                 tuneGrid = knnGrid)

saveRDS(knnModel, 'KNN_solubility_boot.rds')
print('knn Finished')


set.seed(669)
marsModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'earth',
                   tuneGrid = expand.grid(.degree = 1,
                                          .nprune = 2:25))

saveRDS(marsModel, 'mars_solubility_boot.rds')
print('Mars Model Finished')


svmGrid <- expand.grid(sigma = c(0.01, .1, 1),
                       C = c(100:200))

set.seed(669)
svmRModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                   method = 'svmRadial',
                   tuneLength = 15,
                   preProc = c('center', 'scale'))

saveRDS(svmRModel, 'svm_solubility_boot.rds')
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
                   maxit = 1000)

saveRDS(nnetModel, 'nnet_solubility_boot.rds')
print('nnetModel Finished')

set.seed(669)
rpartModel <- train(x = solTrainXtrans,
                    y = solTrainY,
                    method = 'rpart',
                    tuneLength = 30)

saveRDS(rpartModel, 'rpart_solubility_boot.rds')
print('rpart model Finished')

set.seed(669)
ctreeModel <- train(x = solTrainXtrans,
                    y = solTrainY,
                    method = 'ctree',
                    tuneLength = 10)

saveRDS(ctreeModel, 'ctree_solubility_boot.rds')
print('ctree model Finished')

set.seed(669)
treebagModel <- train(x = solTrainXtrans,
                      y = solTrainY,
                      method = 'treebag')

saveRDS(treebagModel, 'treebag_solubility_boot.rds')
print('treebagModel model Finished')

set.seed(669)
rfModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'rf',
                 tuneLength =  10,
                 ntrees = 1000,
                 importance = TRUE)

saveRDS(rfModel, 'rf_solubility_boot.rds')
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
                  verbose = FALSE)

saveRDS(gbmModel, 'gbm_solubility_boot.rds')
print('gbm model Finished')

cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'cubist',
                 tuneGrid = cubistGrid)

saveRDS(cbModel, 'cubist_solubility_boot.rds')
print('cbModel model Finished')

set.seed(669)
mtModel <- train(x = solTrainXtrans,
                 y = solTrainY,
                 method = 'M5')

saveRDS(mtModel, 'M5_solubility_boot.rds')
print('M5 model Finished')