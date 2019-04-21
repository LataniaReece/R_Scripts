library(AppliedPredictiveModeling)
data(permeability)

#predictors = fingerprints , response = permeability 
fingerprints <- data.frame(fingerprints)
permeability <- data.frame(permeability)
#Filtering out the predictors that have low frequency using nearZeroVar 
library(caret)
zerovar_preds <- nearZeroVar(fingerprints, saveMetrics = TRUE)

#zero variance predictors 
zvp <- zerovar_preds[zerovar_preds[,"zeroVar"] > 0, ]

#near zero variance predictors - low frequencies?
nzvp <- zerovar_preds[zerovar_preds[,"zeroVar"] + zerovar_preds[,'nzv'] > 0, ]

#removing variables with nearzerovar predictors 
predictors <- fingerprints[,!(names(fingerprints) %in% rownames(nzvp))]
length(predictors)

#combine the data 
data <- cbind(predictors, permeability = permeability)

#Data Partition 
library(caret)
set.seed(128)
inTrain <- createDataPartition(data$permeability,
                               p = 0.7, 
                               list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]

#PLS model 
ctrl <- trainControl(method = 'repeatedcv',
                     number = 10,
                     repeats = 5)
set.seed(128)
plsFit <- train(permeability ~.,
                data = training, 
                method = 'pls',
                preProc = c('center', 'scale'),
                tuneLength = 20,
                trControl = ctrl)
plsFit
plot(plsFit)

#predict 
pred <- predict(plsFit, testing)
postResample(pred, testing$permeability)

#Ridge Regression
lambda <- 2^seq(-3, 3, length = 100)
set.seed(128)
ridgeFit <- train(permeability ~.,
                  data = training, 
                  method = 'glmnet',
                  tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
ridgeFit
plot(ridgeFit, scales = list(x = list(log = 2)))

#Lasso Regression
set.seed(123)
lassoFit <- train(permeability ~.,
                  data = training, 
                  method = 'glmnet',
                  tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
lassoFit
plot(lassoFit, scales = list(x = list(log = 2)))

#Elastic Regression
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))
set.seed(128)
enetTune <- train(permeability ~., 
                  data = training,
                  method = 'enet', 
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
enetTune 
plot(enetTune)

#Linear Regression 
set.seed(128)
lmFit <- train(permeability ~., 
               data = training, 
               method = 'lm',
               trControl = ctrl)
lmFit
summary(lmFit)

#comparing models 
models <- list(LinearRegression = lmFit, PLS = plsFit, Ridge = ridgeFit, Lasso = lassoFit,
               Elastic = enetTune)
set.seed(128)
resamp <- resamples(models)
summary(resamp, metric = "RMSE")


