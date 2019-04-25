#6.2

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
inTrain = createDataPartition(data$permeability, p=0.8, list = TRUE)
training <- data[inTrain,]
testing <- data[-inTrain,]

# Part (c): Build a PLSR model on this data: 
#
set.seed(0)
pls_model = train(permeability ~.,
                  data= training,
                  method="pls",
                   # the default tuning grid evaluates components 1 ... tuneLength
                   tuneLength=40, 
                   preProcess=c("center","scale"), 
                   trControl=trainControl(method="repeatedcv",repeats=5) )
pls_model
plot(pls_model)
pls_model$times


# Part (d): Predict performance using PLS
#
y_hat = predict( pls_model, newdata=fingerprints_testing )
r2_pls = cor(y_hat,permeability_testing,method="pearson")^2
rmse_pls = sqrt( mean( (y_hat-permeability_testing)^2 ) )
print( sprintf( "%s: Testing R^2= %10.6f; RMSE= %10.6f", "PLS", r2_pls, rmse_pls ) )


# Part (e): Build models to predict permeability using other methods: 
#

# Lets try an Elastic net (this seems to have performed well in past problems) 
#and some other models:
# 
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
enet_model$times

y_hat = predict( enet_model, newdata=fingerprints_testing )
r2_enet = cor(y_hat,permeability_testing,method="pearson")^2
rmse_enet = sqrt( mean( (y_hat-permeability_testing)^2 ) )
print( sprintf( "%s: Testing R^2= %10.6f; RMSE= %10.6f", "ENET", r2_enet, rmse_enet ) )

set.seed(0)
lm_model = train(permeability~., 
                 data = training,
                  method="lm", 
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="repeatedcv",repeats=5) )
lm_model$times

y_hat = predict( lm_model, newdata=fingerprints_testing )
r2_lm = cor(y_hat,permeability_testing,method="pearson")^2
rmse_lm = sqrt( mean( (y_hat-permeability_testing)^2 ) )
print( sprintf( "%s: Testing R^2= %10.6f; RMSE= %10.6f", "LM", r2_lm, rmse_lm ) )

# For rlm we cannot have a singular predictor covariance matrix thus we preprocess with PCA:
# 
set.seed(0)
rlm_model = train(permeability~., 
                  data = training,
                  method="rlm",
                   preProcess=c("pca"),
                   trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(rlm_model, 'permeability_rlmModel.rds')
rlm_model$times

y_hat = predict( rlm_model, newdata=fingerprints_testing )
r2_rlm = cor(y_hat,permeability_testing,method="pearson")^2
rmse_rlm = sqrt( mean( (y_hat-permeability_testing)^2 ) )
print( sprintf( "%s: Testing R^2= %10.6f; RMSE= %10.6f", "RLM", r2_rlm, rmse_rlm ) )
postResample(y_hat, permeability_testing)
# Compare the given models using resamples
#
# resamp = resamples( list(pls=pls_model,enet=enet_model,lm=lm_model,rlm=rlm_model) ) 
# # examples of using this are on EPage 82 
# 
# print( summary(resamp))
# 
# dotplot( resamp, metric="Rsquared" )
# 
# print( summary(diff(resamp)) )
