library(caret)
library(rpart)
library(AppliedPredictiveModeling)
data(permeability)

data <- data.frame(fingerprints, permeability)

# Look for degenerate columns: 
grep('permeability', names(data))
zero_cols = nearZeroVar(data[,-1108])
length(colnames(data)[zero_cols])
data <- data[,-zero_cols]

#Correlation between predictors 
highCorr = findCorrelation(cor(data), cutoff=0.80)
data_independent = data[,-highCorr]

#Data Partiution 
set.seed(10)
inTrain <- createDataPartition(data_independent$permeability, 
                               p = 0.7,
                               list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#MODELS---------
#regression tree
set.seed(10)
rpart_model <- train(permeability ~., 
                     data = training,
                     preProcess = c('center', 'scale'),
                     trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(rpart_model, 'rpart_model_my_8.6.rds')

rpart_pred <- predict(rpart_model, training)
rpartPR <- postResample(rpart_pred, training$permeability)
rmses_training = c(rpartPR [1])
r2s_training = c(rpartPR[2])
methods = c("rpart")

rpart_pred <- predict(rpart_model, testing)
rpartPR <- postResample(rpart_pred, testing$permeability)
rmses_testing = c(rpartPR [1])
r2s_testing = c(rpartPR[2])

#random forest
set.seed(0)
rf_model = train(permeability ~., 
                    data = training,
                    method="rf",
                     preProcess=c("center","scale"),
                     trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(rf_model, 'rf_model_my_8.6.rds')

rf_pred <- predict(rf_model, training)
rfPR <- postResample(rf_pred, training$permeability)
rmses_training = c(rmses_training, rfPR [1])
r2s_training = c(r2s_training, rfPR[2])
methods = c(methods, 'rf')

rf_pred <- predict(rf_model, testing)
rfPR <- postResample(rf_pred, testing$permeability)
rmses_testing = c(rmses_testing, rfPR[1])
r2s_testing = c(r2s_testing, rfPR[2])

#conditional forest
set.seed(0)
cforest_model <- train(permeability ~.,
                       data = training,
                       method = 'cforest',
                       preProcess = c('center', 'scale'),
                       trControl = trainControl(method = 'repeatedcv', repeats = 5))

saveRDS(cforest_model, 'cforest_model_my_8.6.rds')

cforest_pred <- predict(cforest_model, training)
cforestPR <- postResample(cforest_pred, training$permeability)
rmses_training = c(rmses_training, cforestPR[1])
r2s_training = c(r2s_training, cforestPR[2])
methods = c(methods, "cforest")

cforest_pred <- predict(cforest_model, testing)
cforestPR <- postResample(cforest_pred, testing$permeability)
rmses_testing = c(rmses_testing, cforestPR[1])
r2s_testing = c(r2s_testing, cforestPR[2])

#gbm
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

saveRDS(gbm_model, 'gbmModel_my_8.6.rds')

gbm_pred <- predict(gbm_model, training)
gbmPR <- postResample(gbm_pred, training$permeability)
rmses_training = c(rmses_training, gbmPR[1])
r2s_training = c(r2s_training, gbmPR[2])
methods = c(methods, "gbm")

gbm_pred <- predict(gbm_model, testing)
gbmPR <- postResample(gbm_pred, testing$permeability)
rmses_testing = c(rmses_testing, gbmPR[1])
r2s_testing = c(r2s_testing, gbmPR[2])



# Package the results up:
res_training = data.frame( rmse=rmses_training, r2=r2s_training )
rownames(res_training) = methods
training_order = order( -res_training$rmse )
res_training = res_training[ training_order, ]

saveRDS(res_training, 'res_training_ans.8.6.rds')

# Order the dataframe so that the best results are at the bottom:
print( "Final Training Results" ) 
print( res_training )

res_testing = data.frame( rmse=rmses_testing, r2=r2s_testing )
rownames(res_testing) = methods
res_testing = res_testing[ training_order, ] 

saveRDS(res_testing, 'res_testing_ans.8.6.rds')

# Order the dataframe so that the best results for the training set are at the bottom:
print( "Final Testing Results" ) 
print( res_testing )

#RESAMPLES--------------------
resamp = resamples(list(rpart=rpart_model,cforest=cforest_model,rf=rf_model,gbm=gbm_model))
resamp_summary <- summary(resamp)

saveRDS(resamp_summary, 'resamp_summary_ans.8.6.rds')

dotplot(resamp, metric="RMSE")
summary(diff(resamp))

#------------From 6.2
enetModel <- readRDS('enetmodel_ans_6.3.rds')
rlmModel <- readRDS('rlmmodel_ans_6.3.rds')


