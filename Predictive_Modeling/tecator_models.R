#Tecator Models

#Data partition 
library(caret)
data(tecator)

data <- data.frame(absorp, fat = endpoints[,2])

#Data Partition 
set.seed(669)
inTrain <- createDataPartition(data$fat, 
                               p = 0.7,
                               list = FALSE)

training <- data[inTrain,]
testing <- data[-inTrain,]

#Models
ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 5,
                     number = 10)

set.seed(669)
knnModel = train(fat ~., 
                 data = training,
                 method="knn", 
                 preProc=c('center', 'scale'), 
                 tuneLength=10,
                 trControl = ctrl)

print('Knn Model Finished')

set.seed(669)
lmModel <- train(fat ~., 
                   data = training,
                   preProc = c('center', 'scale'),
                   method = 'lm',
                   trControl = ctrl)

print('Linear Model Finished')

set.seed(669)
plsModel <- train(fat ~., 
                  data = training,
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 15,
                  trControl = ctrl)

print('plsModel Finished')

enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1),
                        .fraction =seq(0.05, 1, length = 20))

set.seed(669)
enetModel <- train(fat ~., 
                   data = training,
                   method = 'enet',
                   preProc = c('center', 'scale'),
                   tuneGrid = enetGrid,
                   trControl = ctrl)

saveRDS(enetModel, 'enet_tecator.rds')
print('enetModel Finished')

set.seed(669)
marsModel <- train(fat ~., 
                   data = training, 
                   preProc = c('center', 'scale'),
                    method = 'earth',
                    tuneGrid = expand.grid(.degree = 1,
                                           .nprune = 2:25),
                    trControl = ctrl)
saveRDS(marsModel, 'mars_tecator.rds')
print('Mars Model Finished')

set.seed(669)
svmRModel <- train(fat ~., 
                   data = training,
                   method = 'svmRadial',
                   tuneLength = 15,
                   preProc = c('center', 'scale'),
                   trControl = ctrl)
saveRDS(svmRModel, 'svm_tecator.rds')
print('svmRModel Finished')

nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
                        .size = seq(1,27, by = 2),
                        .bag = FALSE)
set.seed(669)
nnetModel <- train(fat ~., 
                   data = training,
                   method = 'avNNet',
                   tuneGrid = nnetGrid,
                   preProc = c('center', 'scale'),
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 1000,
                   trControl = ctrl)

saveRDS(nnetModel, 'nnet_tecator.rds')
print('nnetModel Finished')

set.seed(669)
rpartModel <- train(fat ~., 
                    data = training,
                    method = 'rpart',
                    tuneLength = 30,
                    trControl = ctrl)

saveRDS(rpartModel, 'rpart_tecator.rds')
print('rpart model Finished')

set.seed(669)
ctreeModel <- train(fat ~., 
                    data = training,
                    method = 'ctree',
                    tuneLength = 10,
                    trControl = ctrl)
saveRDS(ctreeModel, 'ctree_tecator.rds')
print('ctree model Finished')

set.seed(669)
treebagModel <- train(fat ~., 
                      data = training,
                      method = 'treebag',
                      trControl = ctrl)
saveRDS(treebagModel, 'treebag_tecator.rds')
print('treebagModel model Finished')

set.seed(669)
rfModel <- train(fat ~., 
                 data = training,
                 method = 'rf',
                 tuneLength =  10, 
                 ntrees = 1000,
                 importance = TRUE,
                 trControl = ctrl)

saveRDS(rfModel, 'rf_tecator.rds')
print('rf model Finished')

gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10)
set.seed(669)
gbmModel <- train(fat ~., 
                  data = training,
                  method = 'gbm',
                  tuneGrid = gbmGrid,
                  verbose = FALSE,
                  trControl = ctrl)

saveRDS(gbmModel, 'gbm_tecator.rds')
print('gbm model Finished')

cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModel <- train(fat ~., 
                 data = training,
                 method = 'cubist',
                 tuneGrid = cubistGrid,
                 trControl = ctrl)

saveRDS(cbModel, 'cubist_tecator.rds')
print('cbModel model Finished')

set.seed(669)
mtModel <- train(fat ~., 
                 data = training,
                 method = 'M5',
                 trControl = ctrl)
saveRDS(mtModel, 'M5_tecator.rds')
print('M5 model Finished')
