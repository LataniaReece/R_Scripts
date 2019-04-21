library(caret)
data(tecator)

#I believe absorp = predictors, endpoints = outcome(s)
pca_absorp <- prcomp(absorp, scale = TRUE)

#plotting the first 2 components - there should be no correlation
plot(pca_absorp$x[,1], pca_absorp$x[,2]) 
pca.var <- pca_absorp$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

#Scree Plots - first principal component has 98% of variance 
barplot(pca.var[1:15], main = 'Scree Plot',
        xlab = 'Principal Component',
        ylab = 'Percent Variation', 
        ylim = c(0, 100))

barplot(pca.var.per[1:15], main = 'Scree Plot',
        xlab = 'Principal Component',
        ylab = 'Percent Variation', 
        ylim = c(0, 100))

#Data Partition 
#predict fat content - 2nd column in endpoints gonna combine them 
data <- cbind(absorp, fat = endpoints[,2])
dfdata <- data.frame(data)
set.seed(123)
inTrain <- createDataPartition(y = dfdata$fat,
                               p = 0.7, 
                               list = FALSE) 
training <- dfdata[inTrain, ]
testing <- dfdata[-inTrain,]

#linear regression model - checking the effect of collinearity
ctrl = trainControl(method = 'repeatedcv',
                    number = 10,
                    repeats = 3)

set.seed(123)
lmFit <- train(fat ~., 
               data = training, 
               method = 'lm',
               trControl = ctrl)
lmFit
#residuals
#obs vs preds
xyplot(training$fat ~predict(lmFit))
#pred vs resids
xyplot(resid(lmFit) ~ predict(lmFit),
       panel = function(...){
         panel.xyplot(...)
         panel.abline(h = 0)
       })

#robust linear model  - can't have redundants
set.seed(123)
rlmPCA <- train(fat ~.,
                data = training,
                method = 'rlm',
                preProcess = c('center', 'scale', 'pca'),
                trControl = ctrl)
rlmPCA  
plot(rlmPCA)


#pcr (pca and regression)
set.seed(123)
pcrFit <- train(fat ~.,
                data = training, 
                method = 'pcr',
                preProcess = c('center', 'scale'),
                tuneLength = 10,
                trControl = ctrl)
pcrFit
plot(pcrFit)

#pls
set.seed(123)
plsFit <- train(fat ~.,
                data = training, 
                method = 'pls',
                preProcess = c('center', 'scale'),
                tuneLength = 10,
                trControl = ctrl)
plsFit
plot(plsFit)

#Ridge Regression - 'glmnet' , alpha = 0
lambda <- 2^seq(-3, 3, length = 100)
set.seed(123)
ridgeFit <- train(fat ~.,
                  data = training, 
                  method = 'glmnet',
                  tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
ridgeFit
plot(ridgeFit, scales = list(x = list(log = 2)))

#Lasso Regression - 'glmnet', alpha = 1
set.seed(123)
lassoFit <- train(fat ~.,
                  data = training, 
                  method = 'glmnet',
                  tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
lassoFit
plot(lassoFit, scales = list(x = list(log = 2)))

#Elastic Regression - 'glmnet'
set.seed(123)
elastic <- train(fat ~., 
                 data = training, 
                 method = 'glmnet',
                 trControl = ctrl,
                 tuneLength = 10)
elastic
plot(elastic)

#shows alpha on the x axist and RMSE on the y axis
check <- plot(elastic)
check$formula

#Book's way - enetgrid
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))
set.seed(123)
enetTune <- train(fat ~., 
                  data = training,
                  method = 'enet', 
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c('center', 'scale'))
plot(enetTune)
#when lambda is 0 = lasso model, the other values are of the ridge model 

#comparing models 
models <- list(LinearRegression = lmFit, RobustRegression = rlmPCA, PCR = pcrFit, PLS = plsFit,
               Ridge = ridgeFit, Lasso = lassoFit, Elastic = elastic, BElastic = enetTune)
resamp <- resamples(models)
summary(resamp, metric = "RMSE")
#overall this is telling me that lasso regression is the best,
#when you get rid of the correlated predictors

#prediction using the enetTune Model 
pred <- predict(enetTune, testing)
postResample(pred, testing$fat)
