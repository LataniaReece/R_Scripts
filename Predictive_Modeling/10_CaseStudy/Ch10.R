#Ch 10 computation, p 236

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
lmModel <- train(modFormula,
                   data = trainingSet,
                   method = 'lm',
                   trControl = ctrl)
# print('Linear Model Finished')
# # 
set.seed(669)
plsModel <- train(modFormula, data = trainingSet,
                  method = 'pls',
                  preProc = c('center', 'scale'),
                  tuneLength = 15,
                  trControl = ctrl)
# 
# print('plsModel Finished')
# 
# enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1), 
#                         .fraction =seq(0.05, 1, length = 20))
# 
# set.seed(669)
# enetModel <- train(modFormula, data = trainingSet,
#                    method = 'enet',
#                    preProc = c('center', 'scale'),
#                    tuneGrid = enetGrid, 
#                    trControl = ctrl)
# 
# saveRDS(enetModel, 'enet_ch10.rds')
# ?saveRDS
# print('enetModel Finished')
# 
# set.seed(669)
# marsModel <- train(CompressiveStrength ~.,
#                    data = trainingSet,
#                     method = 'earth',
#                     tuneGrid = expand.grid(.degree = 1, 
#                                            .nprune = 2:25),
#                     trControl = ctrl)
# saveRDS(marsModel, 'mars_ch10.rds')
# print('Mars Model Finished')
# 
# set.seed(669)
# svmRModel <- train(CompressiveStrength ~.,
#                    data = trainingSet,
#                    method = 'svmRadial',
#                    tuneLength = 15,
#                    preProc = c('center', 'scale'),
#                    trControl = ctrl)
# saveRDS(svmRModel, 'svm_ch10.rds')
# print('svmRModel Finished')
# 
# nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
#                         .size = seq(1,27, by = 2),
#                         .bag = FALSE)
# set.seed(669)
# nnetModel <- train(CompressiveStrength ~.,
#                    data = trainingSet,
#                    method = 'avNNet',
#                    tuneGrid = nnetGrid,
#                    preProc = c('center', 'scale'),
#                    linout = TRUE,
#                    trace = FALSE,
#                    maxit = 1000,
#                    trControl = ctrl)
# 
# saveRDS(nnetModel, 'nnet_ch10.rds')
# print('nnetModel Finished')
# 
# set.seed(669)
# rpartModel <- train(CompressiveStrength ~., 
#                     data = trainingSet, 
#                     method = 'rpart',
#                     tuneLength = 30,
#                     trControl = ctrl)
# 
# saveRDS(rpartModel, 'rpart_ch10.rds')
# print('rpart model Finished')
# 
# set.seed(669)
# ctreeModel <- train(CompressiveStrength ~.,
#                     data = trainingSet, 
#                     method = 'ctree',
#                     tuneLength = 10, 
#                     trControl = ctrl)
# saveRDS(ctreeModel, 'ctree_ch10.rds')
# print('ctree model Finished')
# 
# set.seed(669)
# mtModel <- train(CompressiveStrength ~., 
#                  data = trainingSet, 
#                  method = 'M5',
#                  trControl = ctrl)
# saveRDS(mtModel, 'M5_ch10.rds')
# print('M5 model Finished')
# 
# set.seed(669)
# treebagModel <- train(CompressiveStrength ~.,
#                       data = trainingSet,
#                       method = 'treebag',
#                       trControl = ctrl)
# saveRDS(treebagModel, 'treebagModel_ch10.rds')
# print('treebagModel model Finished')
# 
# set.seed(669)
# rfModel <- train(CompressiveStrength ~., 
#                  data= trainingSet,
#                  method = 'rf',
#                  tuneLength =  10, 
#                  ntrees = 1000,
#                  importance = TRUE,
#                  trControl = ctrl)
# 
# saveRDS(rfModel, 'rf_ch10.rds')
# print('rf model Finished')
# 
# gbmGrid <- expand.grid(interaction.depth = seq(1,7, by = 2),
#                        n.trees = seq(100, 1000, by = 50),
#                        shrinkage = c(0.01, 0.1),
#                        n.minobsinnode = 10)
# set.seed(669)
# gbmModel <- train(CompressiveStrength ~., 
#                   data = trainingSet,
#                   method = 'gbm',
#                   tuneGrid = gbmGrid,
#                   verbose = FALSE,
#                   trControl = ctrl)
# 
# saveRDS(gbmModel, 'gbm_ch10.rds')
# print('gbm model Finished')
# 
cubistGrid <- expand.grid(committees = c(1,5,10,50,75,100),
                          neighbors = c(0,1,3,5,7,9))
set.seed(669)
cbModel <- train(CompressiveStrength ~.,
                 data = trainingSet,
                 method = 'cubist',
                 tuneGrid = cubistGrid,
                 trControl = ctrl)
# 
saveRDS(cbModel, 'cubist_ch10.rds')
# print('cbModel model Finished')

#Loading RDS Models

enetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\enet_ch10.rds')
marsModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\mars_ch10.rds')
svmRModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\svm_ch10.rds')
nnetModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\nnet_ch10.rds')
rpartModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\rpart_ch10.rds')
ctreeModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\ctree_ch10.rds')
mtModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\M5_ch10.rds')
treebagModel <- readRDS('treebagModel_ch10.rds')
rfModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\rf_ch10.rds')
gbmModel <- readRDS('C:\\Users\\reece\\Desktop\\R_Scripts\\Predictive_Modeling\\10_CaseStudy\\gbm_ch10.rds')
cbModel <- readRDS('cubist_ch10.rds')

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
#Parallel Coordinate Plots (p231)
#Plot the RMSE values 
parallelplot(allResamples, metric = 'RMSE')
#Using R-Squared
parallelplot(allResamples, metric = 'Rsquared')

#Predictions - with best models (nnet, gbm, cubist)
nnetPredictions <- predict(nnetModel, testSet)
gbmPrediction <- predict(gbmModel, testSet)
cbPredictions <- predict(cbModel, testSet)

#Predicting optimal mixtures, use 28-day data and generate a set of random starting points from trainingset
age28Data <- subset(trainingSet, Age == 28)
#remove the age and compressive strength columns and then center and scale the predcitor columns 
pp1 <- preProcess(age28Data[,-(8:9)], c('center', 'scale'))
scaledTrain <- predict(pp1, age28Data[,1:7])
set.seed(91)
startMixture <- sample(1:nrow(age28Data), 1)
starters <- scaledTrain[startMixture, 1:7]

#using maximum dissimilarity sampling method to select 14 more mixtures to complete 
#diverse set of starting points to search for algorithms
pool <- scaledTrain
index <- maxDissim(starters, pool, 14)
startPoints <- c(startMixture, index)
starters <- age28Data[startPoints, 1:7]
#Now we have 15 samples 

#Note, all seven mixture proportions should add to one, so remove water in order to make sure this happens 
##remove water 
startingValues <- starters[,-4]

#we are going to use function optim to maximize compressive strength, 
#first
#custom R function to translate candidate mixture to prediction 
#function can find settings to minimize a function so it will return a negative of the compressive strength
#the function checks that (1) proportions are between 0 and 1 and
#(2) the proportion of water does not fall below 5%
#if these conditions are violated, returns 10^38 - which the search procedure will avoid (optim min func)

modelPrediction <- function(x, predictive_model)
{
  if(x[1] < 0 | x[1] > 1) return (10^38)
  if(x[2] < 0 | x[2] > 1) return (10^38)
  if(x[3] < 0 | x[3] > 1) return (10^38)
  if(x[4] < 0 | x[4] > 1) return (10^38)
  if(x[5] < 0 | x[5] > 1) return (10^38)
  if(x[6] < 0 | x[6] > 1) return (10^38)
  
  x <- c(x, 1 - sum(x))
  
  #check the water range 
  if(x[7] < 0.05) return (10^38)
  
  #convert the vector to a data frame, assign names 
  #and fix age at 28 days 
  tmp <- as.data.frame(t(x))
  names(tmp) <- c('Cement', 'BlastFurnaceSlag', 'FlyAsh', 
                  'Superplasticizer', 'CoarseAggregate',
                  'FineAggregate', 'Water')
  tmp$Age <- 28
  #Get the model prediction, square them back to get original units, 
  #then return the negative of the result
  -predict(predictive_model, tmp)
}

#First the Cubist Model is used 
cbResults <- startingValues
cbResults$Water <- NA
cbResults$Prediction <- NA

##Loop over each starting points and conduct the search 
for(i in 1:nrow(cbResults))
{
  results <- optim(unlist(cbResults[i, 1:6]),
                   modelPrediction,
                   method = 'Nelder-Mead',
                   #Use methdd = 'SANN' for simulated annealing
                   control = list(maxit = 5000),
                   #the next option is passed to the modelPrediction function
                   predictive_model = cbModel)
  
  #save the predicted compressive strength 
  cbResults$Prediction[i] <- -results$value
  ##Also save the final mixture values 
  cbResults[i, 1:6] <- results$par
}

##Calculate the water proportion
cbResults$Water <- 1 - apply(cbResults[,1:6], 1, sum)
#Keep top 3 mixtures 
cbResults <- cbResults[order(-cbResults$Prediction),][1:3,]
cbResults$Model <- 'Cubist'
saveRDS(cbResults, 'ch10_cbresults_optim.rds')
#Now the neural networds 
nnetResults <- startingValues 
nnetResults$Water <- NA
nnetResults$Prediction <- NA
for (i in 1:nrow(nnetResults))
{
  results <- optim(unlist(nnetResults[i, 1:6]),
                   modelPrediction,
                   method = 'Nelder-Mead',
                   control = list(maxit = 5000),
                   predictive_model = nnetModel)
  nnetResults$Prediction[i] <- -results$value
  nnetResults[i, 1:6] <- results$par
}

nnetResults$Water <- 1 - apply(nnetResults[,1:6], 1, sum)
nnetResults <- nnetResults[order(-nnetResults$Prediction),][1:3,]
nnetResults$Model <- 'NNet'
saveRDS(nnetResults, 'ch10_nnetresults_optim.rds')

#PCA on 28-day-old mistures 
pp2 <- preProcess(age28Data[,1:7], 'pca')
pca1 <- predict(pp2, age28Data[,1:7])
pca1$Data <- 'Training Set'
#Label which data points were used to start searches 
pca1$Data[startPoints] <- 'Starting Values'

#Project the new mixtures in the same way (make sure to reorder the columns to match the order of the 
#age28Data object)

pca3 <- predict(pp2, cbResults[, names(age28Data[, 1:7])])
pca3$Data <- 'Cubist'
pca4 <- predict(pp2, nnetResults[, names(age28Data[, 1:7])])
pca4$Data <- 'Neural Network'

##Combine the data, determine the axis ranges and plot 
pcaData <- rbind(pca1, pca3, pca4)
pcaData$Data <- factor(pcaData$Data,
                       levels = c('Training Set', 'Starting Values', 
                                  'Cubist', 'Neural Network'))
lim <- extendrange(pcaData[,1:2])
xyplot(PC2 ~ PC1, data = pcaData, groups = Data,
       auto.key = list(columns = 2),
       xlim = lim, ylim = lim, 
       type = c('g', 'p'))





