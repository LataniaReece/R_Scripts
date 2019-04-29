library(caret)
library(AppliedPredictiveModeling)

set.seed(0)

data(permeability)
data <- data.frame(fingerprints, permeability)

set.seed(0)
inTrain = createDataPartition(data$permeability, p=0.8, list = FALSE)
training <- data[inTrain,]                   
testing <- data[-inTrain,]

set.seed(0)
pls_modelNT = train(permeability ~.,
                  data= training,
                  method="pls",
                  # the default tuning grid evaluates components 1 ... tuneLength
                  tuneLength=40, 
                  preProcess=c("center","scale"), 
                  trControl=trainControl(method="repeatedcv",repeats=5) )

set.seed(0)
lm_modelNT = train(permeability~., 
                 data = training,
                 method="lm", 
                 preProcess=c("center","scale"),
                 trControl=trainControl(method="repeatedcv",repeats=5) )

# enetGrid = expand.grid(.lambda=seq(0,1,length=20), .fraction=seq(0.05, 1.0, length=20))
# set.seed(0)
# enet_modelNT = train(permeability~., 
#                    data = training, 
#                    method="enet",
#                    # fit the model over many penalty values
#                    tuneGrid = enetGrid,
#                    preProcess=c("center","scale"),
#                    trControl=trainControl(method="repeatedcv",repeats=5) )
# saveRDS(enet_modelNT, 'permeability_enet_modelNT.rds')

# set.seed(0)
# rlm_modelNT = train(permeability~., 
#                   data = training,
#                   method="rlm",
#                   preProcess=c("pca"),
#                   trControl=trainControl(method="repeatedcv",repeats=5) )

# saveRDS(rlm_modelNT, 'permeability_rlmModelNT.rds')

set.seed(0)
rpart_modelNT <- train(permeability ~., 
                     data = training,
                     preProcess = c('center', 'scale'),
                     trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(rpart_modelNT, 'permeability_rpartNT.rds')

set.seed(0)
rf_modelNT = train(permeability ~., 
                 data = training,
                 method="rf",
                 preProcess=c("center","scale"),
                 trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(rf_modelNT, 'permeability_rfModelNT.rds')

set.seed(0)
cforest_modelNT <- train(permeability ~.,
                       data = training,
                       method = 'cforest',
                       preProcess = c('center', 'scale'),
                       trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(cforest_modelNT, 'permeability_cforestNT.rds')

gbmGrid = expand.grid(interaction.depth = seq(1,7, by=2),
                      n.trees = seq(100, 1000, by=100),
                      shrinkage = c(0.01, 0.1),
                      n.minobsinnode = 10)
set.seed(0)
gbm_modelNT = train(permeability ~., data = training,
                  method="gbm",
                  preProcess=c("center","scale"),
                  tuneGrid = gbmGrid, 
                  trControl=trainControl(method="repeatedcv",repeats=5),
                  verbose=FALSE)

saveRDS(gbm_modelNT, 'permeability_gbmNT.rds')



#No cv-------------------------------------------------------------------------------------
# A K-NN model:

preProc_Arguments = c("center","scale")
# 
set.seed(0)
knnModelNT = train(permeability ~., 
                 data = training,
                 method="knn", 
                 preProc=preProc_Arguments, 
                 tuneLength=30,
                 trControl = trainControl(method = 'repeatedcv', repeats = 5))

print('Finished knn Model')
saveRDS(knnModelNT, 'permeabilit_knnNT.rds')

nnGrid = expand.grid( decay=c(0,0.01,0.1),
                      size=1:10)
set.seed(0)
nnetModelNT = train(permeability ~.,
                  data = training,
                  method="nnet",
                  preProc=preProc_Arguments,
                  linout=TRUE,
                  tuneGrid = nnGrid,
                  trace=FALSE,
                  MaxNWts=10 * (ncol(training[,-389])+1) + 10 + 1,
                  maxit=500,
                  trControl = trainControl(method = 'repeatedcv', repeats = 5))
saveRDS(nnetModelNT, 'permeability_nnetNT.rds')
print('Finished nnet Model')
# 
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
set.seed(0)
marsModelNT = train(permeability ~ .,
                  data = training,
                  method="earth",
                  preProc=preProc_Arguments,
                  tuneGrid=marsGrid, 
                  trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(marsModelNT, 'permeability_marsModelNT.rds')
print('Finished mars Model')

set.seed(0)
svmModelNT = train(permeability~., 
                 data = training,
                 method="svmRadial",
                 preProc=preProc_Arguments,
                 tuneLength=20,
                 trControl = trainControl(method = 'repeatedcv', repeats = 5))
saveRDS(svmModelNT, 'permeability_svmNT.rds')

set.seed(0)
mtModelNT <- train(permeability~., 
                 data = training,
                 method = 'M5',
                 trControl = trainControl(method = 'repeatedcv', repeats = 5))
saveRDS(mtModelNT, 'permeability_M5NT.rds')
print('M5 model Finished')

#------------------------


data <- data.frame(fingerprints, permeability)
# Part (b):
# 
zero_cols = nearZeroVar( fingerprints )
print( sprintf("Found %d zero variance columns from %d",length(zero_cols), dim(fingerprints)[2]))
data = data[,-zero_cols]

print(sprintf('Now using %d variables in training',length(data)))

set.seed(0)
mtModel <- train(permeability~., 
                 data = training,
                 method = 'M5',
                 trControl = trainControl(method = 'repeatedcv', repeats = 5))
saveRDS(mtModel, 'permeability_M5.rds')
print('M5 model Finished')
