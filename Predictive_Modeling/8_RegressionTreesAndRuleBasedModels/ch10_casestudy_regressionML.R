library(caret)
print('check')
library(AppliedPredictiveModeling)
data(concrete)

library(plyr)
averaged <- ddply(mixtures,
                  .(Cement, BlastFurnaceSlag, FlyAsh, Water,
                    Superplasticizer, CoarseAggregate,
                    FineAggregate, Age),
                  function(x) c(CompressiveStrength =
                                  mean(x$CompressiveStrength)))
set.seed(975)
forTraining <- createDataPartition(averaged$CompressiveStrength,
                                   p = 3/4, list = FALSE)
trainingSet <- averaged[forTraining,]
testSet <- averaged[-forTraining,]

#adding interactions into the model 
#(.)^2 = expands into a model with all linear terms and all two-factor interaction 
#I() quadratic terms are in here to tell R that squaring of the predictors should be done arithmetically
#and not symbolically 

modFormula <- paste('CompressiveStrength ~ (.)^2 + I(Cement^2) +',
                    'I(BlastFurnaceSlag^2) + I(FlyAsh^2) +I(Water^2)+',
                    'I(Superplasticizer^2) + I(CoarseAggregate^2)+',
                    'I(FineAggregate^2) + I(Age^2)')
modFormula <- as.formula(modFormula)

ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
linearReg <- train(modFormula, 
                   data = trainingSet,
                   method = 'lm',
                   trControl = ctrl)
print('Linear Model Finished')

set.seed(669)
plsModel <- train(modFormula, data = trainingSet, 
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 15,
                  trControl = ctrl)

print('plsModel Finished')

enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1), 
                        .fraction =seq(0.05, 1, length = 20))

set.seed(669)
enetModel <- train(modFormula, data = trainingSet,
                   method = 'enet',
                   preProc = c('center', 'scale'),
                   tuneGrid = enetGrid, 
                   trControl = ctrl)

saveRDS(enetModel, 'enet_ch10.rds')
print('enetModel Finished')

set.seed(669)
marsModel <- train(CompressiveStrength ~.,
                   data = trainingSet,
                    method = 'earth',
                    tuneGrid = expand.grid(.degree = 1, 
                                           .nprune = 2:25),
                    trControl = ctrl)
saveRDS(marsModel, 'mars_ch10.rds')
print('Mars Model Finished')

set.seed(669)
svmRModel <- train(CompressiveStrength ~.,
                   data = trainingSet,
                   method = 'svmRadial',
                   tuneLength = 15,
                   preProc = c('center', 'scale'),
                   trControl = ctrl)
saveRDS(svmRModel, 'svm_ch10.rds')
print('svmRModel Finished')

nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
                        .size = seq(1,27, by = 2),
                        .bag = FALSE)
set.seed(669)
nnetModel <- train(CompressiveStrength ~.,
                   data = trainingSet,
                   method = 'avNNet',
                   tuneGrid = nnetGrid,
                   preProc = c('center', 'scale'),
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 1000,
                   trControl = ctrl)

saveRDS(nnetModel, 'nnet_ch10.rds')
print('nnetModel Finished')

set.seed(669)
rpartModel <- train(CompressiveStrength ~., 
                    data = trainingSet, 
                    method = 'rpart',
                    tuneLength = 30,
                    trControl = ctrl)

saveRDS(rpartModel, 'rpart_ch10.rds')
print('rpart model Finished')

set.seed(669)
ctreeModel <- train(CompressiveStrength ~.,
                    data = trainingSet, 
                    method = 'ctree',
                    tuneLength = 10, 
                    trControl = ctrl)
saveRDS(ctreeModel, 'ctree_ch10.rds')
print('ctree model Finished')

set.seed(669)
mtModel <- train(CompressiveStrength ~., 
                 data = trainingSet, 
                 method = 'M5',
                 trControl = ctrl)
saveRDS(mtModel, 'M5_ch10.rds')
print('M5 model Finished')

set.seed(669)
treebagModel <- train(CompressiveStrength ~., 
                      data = trainingSet, 
                      method = 'treebag',
                      trControl = ctrl)

print('treebagModel model Finished')

set.seed(669)
rfModel <- train(CompressiveStrength ~., 
                 data= trainingSet,
                 method = 'rf',
                 tuneLength =  10, 
                 ntrees = 1000,
                 importance = TRUE,
                 trControl = ctrl)

saveRDS(rfModel, 'rf_ch10.rds')
print('rf model Finished')

gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10)
set.seed(669)
gbmModel <- train(CompressiveStrength ~., 
                  data = trainingSet,
                  method = 'gbm',
                  tuneGrid = gbmGrid,
                  verbose = FALSE,
                  trControl = ctrl)

saveRDS(gbmModel, 'gbm_ch10.rds')
print('gbm model Finished')

cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModel <- train(CompressiveStrength ~., 
                 data = trainingSet, 
                 method = 'cubist',
                 tuneGrid = cubistGrid,
                 trcontrol = ctrl)

saveRDS(cbModel, 'cubist_ch10.rds')
print('cbModel model Finished')
