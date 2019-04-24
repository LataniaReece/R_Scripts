
#rpart = CART methodology splits 
#ctree = conditional inference splits 

#just showing the functions 
library(rpart)
rpartTree <- rpart(y ~., data = trainData)
library(partykit)
ctreeTree <- ctree(y~., data = trainData)

#rpart with train 
#to tune over cp - rpart 
#to tune over maximum depth = rpart2 
library(AppliedPredictiveModeling)
data(solubility)
library(caret)

set.seed(100)
rpartTune <- train(solTrainXtrans, solTrainY,
                   method = 'rpart2',
                   tuneLength = 10,
                   trControl = trainControl(method = 'cv'))
rpartTune
#see p 214 to see the tuning parameters for c tree
#?ctree_control

#to plot tree using partykit, convert to part tree than plot 
library(partykit)
rpartTree2 <- as.party(rpartTune$finalModel)
plot(rpartTree2)

#MODEL TREES - p 214 
#M5P ???ts the model tree, while M5Rules uses the rulebased version. 
library(RWeka)
m5tree <- M5P(y~., data = trainData)#Jave problem

#Model trees and rule based trees with caret 
set.seed(100)
m5Tune <- train(solTrainXtrans, solTrainY,
                method = 'M5',
                trControl = trainControl(method = 'cv'),
                control = Weka_control(M= 10))
m5Tune
plot(m5Tune)

saveRDS(m5Tune, 'm5tune_example8.rds')

#BAGGED TREES p 215
library(ipred)
baggedTree <- ipredbagg(solTrainY, solTrainXtrans)
#or
#baggedTree <- bagging(y~., data =trainData)

#RANDOM FORESTS
library(randomForest)
rfModel <- randomForest(solTrainXtrans, solTrainY)
#or 
#rfModel <- randomforest(y ~., data = trainData)

#rf parameters - mtry - number of predictors, importance, ntrees
library(randomForest)
rfModel <- randomForest(solTrainXtrans, solTrainY,
                        importance = TRUE,
                        ntrees = 1000)
plot(rfModel)

#BOOSTED TREES p 216
library(gbm)
gbmModel <- gbm.fit(solTrainXtrans, solTrainY, 
                    distribution = 'gaussian')
#or
#gbmModel <- gbm(y ~., data = trainData, distribution = 'gaussian)

#using gbm 
# n.minobsinnode means when should I stop, n.minobsinnode = 10 mean stop when there are 10 observations
# in the terminal node https://stats.stackexchange.com/questions/30645/role-of-n-minobsinnode-parameter-of-gbm-in-r

gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
                       n.trees = seq(100, 1000, by = 5),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10)
set.seed(100)
gbmTune <- train(solTrainXtrans, solTrainY,
                 method = 'gbm',
                 tuneGrid = gbmGrid,
                 verbose = FALSE)
saveRDS(gbmTune, 'gbmTune_example8.rds')

#Cubist p 217
library(Cubist)
#has argument committees to fit multiple models, not using it in this case
cubistMod <- cubist(solTrainXtrans, solTrainY)
predict(cubistMod, solTestXtrans)

#using train function

cubistTuned <- train(solTrainXtrans, solTrainY, method = 'cubist')

saveRDS(cubistTuned, 'cubeTuned_exampled8.rds')
