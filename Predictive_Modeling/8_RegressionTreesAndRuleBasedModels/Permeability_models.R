library(caret)
library(AppliedPredictiveModeling)

set.seed(0)

data(permeability)
data <- data.frame(fingerprints, permeability)
# Part (b):
# 
zero_cols = nearZeroVar( fingerprints )
print( sprintf("Found %d zero variance columns from %d",length(zero_cols), dim(fingerprints)[2]))
data = data[,-zero_cols] # drop these zero variance columns 

# Split this data into training and testing sets:
#
set.seed(0)
inTrain = createDataPartition(data$permeability, p=0.8, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

set.seed(0)
pls_model = train(permeability ~.,
                  data= training,
                  method="pls",
                  # the default tuning grid evaluates components 1 ... tuneLength
                  tuneLength=40, 
                  preProcess=c("center","scale"), 
                  trControl=trainControl(method="repeatedcv",repeats=5) )

set.seed(0)
lm_model = train(permeability~., 
                 data = training,
                 method="lm", 
                 preProcess=c("center","scale"),
                 trControl=trainControl(method="repeatedcv",repeats=5) )

enetGrid = expand.grid(.lambda=seq(0,1,length=20), .fraction=seq(0.05, 1.0, length=20))
set.seed(0)
enet_model = train(permeability~., 
                   data = training, 
                   method="enet",
                   # fit the model over many penalty values
                   tuneGrid = enetGrid,
                   preProcess=c("center","scale"),
                   trControl=trainControl(method="repeatedcv",repeats=5) )
saveRDS(enet_model, 'permeability_enet_model.rds')

library(elasticnet)
?enet
set.seed(0)
rlm_model = train(permeability~., 
                  data = training,
                  method="rlm",
                  preProcess=c("pca"),
                  trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(rlm_model, 'permeability_rlmModel.rds')

set.seed(0)
rpart_model <- train(permeability ~., 
                     data = training,
                     preProcess = c('center', 'scale'),
                     trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(rpart_model, 'permeability_rpart.rds')

set.seed(0)
rf_model = train(permeability ~., 
                 data = training,
                 method="rf",
                 preProcess=c("center","scale"),
                 trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(rf_model, 'permeability_rfModel.rds')

set.seed(0)
cforest_model <- train(permeability ~.,
                       data = training,
                       method = 'cforest',
                       preProcess = c('center', 'scale'),
                       trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(cforest_model, 'permeability_cforest.rds')

gbmGrid = expand.grid(interaction.depth = seq(1,7, by=2),
                      n.trees = seq(100, 1000, by=100),
                      shrinkage = c(0.01, 0.1),
                      n.minobsinnode = 10)
set.seed(0)
gbm_model = train(permeability ~., data = training,
                  method="gbm",
                  preProcess=c("center","scale"),
                  tuneGrid = gbmGrid, 
                  trControl=trainControl(method="repeatedcv",repeats=5),
                  verbose=FALSE)

saveRDS(gbm_model, 'permeability_gbm.rds')



#No cv-------------------------------------------------------------------------------------
# A K-NN model:

preProc_Arguments = c("center","scale")
# 
set.seed(0)
knnModel = train(permeability ~., 
                 data = training,
                 method="knn", 
                 preProc=preProc_Arguments, 
                 tuneLength=10,
                 trControl = trainControl(method = 'repeatedcv', repeats = 5))

# print('Finished knn Model')

nnGrid = expand.grid( .decay=c(0,0.01,0.1),
                      .size=1:10,
                      .bag=FALSE )
set.seed(0)
nnetModel = train(permeability ~.,
                  data = training,
                  method="nnet",
                  preProc=preProc_Arguments,
                  linout=TRUE,
                  trace=FALSE,
                  MaxNWts=10 * (ncol(training[,-389])+1) + 10 + 1,
                  maxit=500,
                  trControl = trainControl(method = 'repeatedcv', repeats = 5))
saveRDS(nnetModel, 'permeability_nnet.rds')
?nnet
# print('Finished nnet Model')
# 
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
 set.seed(0)
 marsModel = train(permeability ~ .,
                   data = training,
                   method="earth",
                   preProc=preProc_Arguments,
                   tuneGrid=marsGrid, 
                   trControl = trainControl(method = 'repeatedcv', repeats = 5))
 
 ?earth
 saveRDS(marsModel, 'permeability_marsModel.rds')
# print('Finished mars Model')

set.seed(0)
svmModel = train(permeability~., 
                  data = training,
                  method="svmRadial",
                  preProc=preProc_Arguments,
                  tuneLength=20,
                 trControl = trainControl(method = 'repeatedcv', repeats = 5))
saveRDS(svmModel, 'permeability_svm.rds')
# print('Finished svm Model')

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






