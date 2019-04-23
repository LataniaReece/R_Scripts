#p 161

#Data partition 
library(caret)
data(tecator)

data <- data.frame(absorp, fat = endpoints[,2])

#Data Partition 
set.seed(1818)
inTrain <- createDataPartition(data$fat, 
                               p = 0.7,
                               list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#standardize the data
library(robustHD)
training <- standardize(training)
testing <- standardize(testing)

#Neural Networks --------------

grep('fat', names(data))
#Below we have a model with 5 hidden units - size = 5
library(nnet)
set.seed(1818)
nnetFit <- nnet(training[,-101], training$fat,
                size = 5, 
                decay = 0.01,
                linout = TRUE,
                trace = FALSE,
                maxit = 500,
                MaxNWts = 5 * (ncol(training[,-101]) + 1) + 5 + 1)
library(NeuralNetTools)
plotnet(nnetFit)

#prediction 
preds <- predict(nnetFit, testing)

?avNNet
#model averaging - use caret package - repeats = how many models to average
set.seed(1818)
nnetAvg <- avNNet(training[,-101], training$fat,
                  size = 5,
                  decay = 0.01, 
                  repeats = 5, 
                  linout = TRUE,
                  trace = FALSE,
                  maxit = 500,
                  MaxNWts = 5 * (ncol(training[,-101]) + 1) + 5 + 1)
nnetAvg
plotnet(nnetFit)
#predictions 
preds <- predict(nnetAvg, testing)

#using caret function - choosing the number of hidden layers and amount of weight decay via resampling
#can use method 'nnet' or method 'avNNet'
#first must ensure that maximin pairwise correlation between predictors is 0.75

#using solubility data for this 
library(AppliedPredictiveModeling)
data(solubility)
tooHigh <- findCorrelation(cor(solTrainXtrans), cutoff = 0.75)
trainXnet <- solTrainXtrans[, -tooHigh]
testXnet <- solTestXtrans[,-tooHigh]

#caret Neural Network # this took forever so I did not look at the results 
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
                        .size = c(1:10),
                        .bag = FALSE)
ctrl <- trainControl(method = 'cv',
                     number = 10)
set.seed(1818)
nnetTune <- train(trainXnet, solTrainY, 
                  method = 'avNNet',
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  preProcess = c('center', 'scale'),
                  linout = TRUE,
                  maxit = 100)

#MARS-----------------------------
marsFit <- earth(solTrainXtrans, solTrainY)
marsFit 
summary(marsFit)

#below plots the predicted relationship between the outcome and the continuous predictors
#interpretation on p.152
plotmo(marsFit, margin = c(1,1,1,1))

#Mars using train function 
?earth
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(1818)
marsTuned <- train(solTrainXtrans, solTrainY,
                   method = 'earth',
                   tuneGrid = marsGrid,
                   trControl = trainControl(method = 'cv'))
marsTuned
saveRDS(marsTuned, 'mars_solubility.RDS')
plot(marsTuned)
#variable importance 
varImp(marsTuned)

#SVM------------------------------------------------------
library(kernlab)
#note that ksvm only takes matrices and not data frames
predictors <- as.matrix(solTrainXtrans)
outcome <- as.matrix(solTrainY)
set.seed(18)
svmFit <- ksvm(x = predictors, y = outcome,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
svmFit

#using the kernel function
set.seed(1818)
svmRTuned <-  train(solTrainXtrans, solTrainY,
                  method = 'svmRadial',
                  preProc = c('center', 'scale'),
                  tuneLength = 14,
                  trControl = trainControl(method = 'cv'))
svmRTuned
svmRTuned$finalModel
#KNN-----------------------------------------------------
#first remove sparse and unbalanced variables first 
knnDescr <- solTrainXtrans[,-nearZeroVar(solTrainXtrans)]
set.seed(100)
knnTune <- train(knnDescr,
                 solTrainY,
                 method = 'knn',
                 preProcess = c('center', 'scale'),
                 tuneGrid = data.frame(.k = 1:20),
                 trControl = trainControl(method = 'cv'))
knnTune
