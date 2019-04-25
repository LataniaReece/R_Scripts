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

