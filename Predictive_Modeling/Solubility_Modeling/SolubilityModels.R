#Data Description P 102 (117)
#Predicting solubility using chemical structures 
#Data consists of: 
  #1267 total compounds 
  #208 binary 'fingerprints' that indicate the presenc eor absence of a particular chemical structure 
  #16 Count descriptors such as number of bonds or number of bromine atoms 
  #4 Continous descriptors such as molecular weight or surface area
#data already consists of transformed predictors, will screen both sets of predictors
  #They used Box Cox transformation since continuous data was skewed 
#outcome of data measured on log10 scale and ranged from -11 to 1.6 with an average log solubility of -2.7

#Train/Test Split - random sampling, 951 train 316 test
library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)

#Screening Predictors - starting with 228 predictors----------------
#SolTrainX
#1st. since fingerprint predictors are categorical and binary, preprocessing won't do much
#selecting continuous predictors 

library(dplyr)
continuous_predictors <- solTrainX %>%
  select(-(starts_with('F')))

#Correlations of predictors 
correlations <- cor(continuous_predictors)
library(corrplot)
corrplot(correlations, order = 'hclust')

  #PCA analysis can also tell you about correlations. Can the data be summed in smaller dimensions?
pca <- prcomp(continuous_predictors, 
              center = TRUE, scale = TRUE)
pcaVar <- round(pca$sdev^2, 4)
pcaVar_percent <- round(pcaVar/sum(pcaVar)* 100, 1)
pcaVar_percent_cum <- cumsum(pcaVar_percent)

    #Scree Plot 
plot(1:10, pcaVar_percent[1:10], 
     type = 'b',
     main = 'Scree Plot',
     xlab = 'Components',
     ylab = 'Variance')
axis(side = 1, at = (1:10))
axis(side = 2, at = seq(0, 100, by = 5))

#Skewness 
library(e1071)
skew_values <- round(apply(continuous_predictors, 2, skewness), 2)
skew_values <- data.frame(variables = names(skew_values), value = skew_values, 
                          row.names = 1:length(skew_values))
highskew <- skew_values[abs(skew_values$value) > 1.5,]
head(highskew[order(-abs(highskew$value)),], n = 15)

  #Applying BoxCox - make sure to add a constant or it will just ignore negative and zero values

library(caret)
set.seed(0)
boxcox <- preProcess(continuous_predictors + 1, method = 'BoxCox')
continuous_predictorsTrans <- predict(boxcox, continuous_predictors + 1)


#Categorical Predictors (208 of them)
#imabalances?
categorical_predictors <- solTrainX %>%
  select(starts_with('F'))

#Frequency Distribution into a df
freqDist <- apply(categorical_predictors, 2, function(x) prop.table(table(x)))
freqDist <- round(freqDist,2)

df_freqDist <- data.frame(t(check))
imbalancedClasses <- df_freqDist[df_freqDist$X0 <= 0.2 | df_freqDist$X1 <= 0.2,]
#140 of them are imbalanced 

#Models----------------------------------------------------
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


knnGrid <- expand.grid(k = 1:30)
set.seed(669)

knnModel = train(x = solTrainXtrans,
                 y = solTrainY,
                 method="knn",
                 preProc= c('center', 'scale'),
                 tuneGrid = knnGrid,
                 trControl = ctrl)
saveRDS(knnModel, 'KNN_solubility.rds')

set.seed(669)
marsModel <- train(x = solTrainXtrans,
                   y = solTrainY,
                    method = 'earth',
                    tuneGrid = expand.grid(.degree = 1,
                                           .nprune = 2:25),
                    trControl = ctrl)
saveRDS(marsModel, 'mars_solubility.rds')
print('Mars Model Finished')


svmGrid <- expand.grid(sigma = c(0.01, .1, 1),
                       C = c(100:200))
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
ctreeModel2 <- train(x = solTrainXtrans,
                    y = solTrainY,
                    method = 'ctree',
                    tuneGrid = expand.grid(mincriterion = seq(0, 0.1, by = 0.005)),
                    trControl = ctrl)
saveRDS(ctreeModel2, 'ctree2_solubility.rds')




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





