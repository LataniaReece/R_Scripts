#wesbite: https://youtu.be/_3xMSbIde2I

library(mlbench)

#Data
data("BostonHousing")
data <- BostonHousing
library(psych)
pairs.panels(data[c(-4, -14)], cex =2)

#Data Partition 
library(caret)
set.seed(123)
inTrain <- createDataPartition(y = data$medv, 
                               p = 0.7, list = FALSE)
training <- data [inTrain, ]
testing <- data[-inTrain,]

#Custom Control Parameters 
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5, 
                       verboseIter = T)

#Linear Model
set.seed(1234)
lm <- train(medv ~., 
            data = training, 
            method = "lm",
            trControl = custom)

#results
lm$results
lm
summary(lm)
plot(lm$finalModel)

#Ridge Regression
set.seed(1234)
ridge <- train(medv ~., 
               data = training, 
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.001, 1, length = 5)),
               
               trControl = custom)     
#Plot results 
plot(ridge) #shows which lamba had the lowest error
ridge
plot(ridge$finalModel, xvar = "lambda", label = T) 
#as you increase lamda, the size of the coeficients decrease
plot(ridge$finalModel, xvar = "dev", label = T)
plot(varImp(ridge, scale = T))

#Lasso Regression 
set.seed(1234)
lasso <- train(medv~., 
               data = training, 
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 1, length = 5)),
               trControl = custom)
#Plot Results 
plot(lasso) #shows which lamba had the lowest error
lasso
plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = "dev", label = T)
plot(varImp(lasso, scale = T))

#Elastic Net Regression
set.seed(1234)
en <- train(medv~.,
            data = training,
            method = "glmnet",
            tuneGrid = expand.grid(alpha = seq(0,10, length = 10),
                                   lambda = seq(0.001, 1, length = 5)),
            trControl = custom)

#Plot Results 
plot(en)
plot(en$finalModel, xvar = "lambda", label = T)
plot(en$finalModel, xvar = "dev", label = T)
plot(varImp(en))

#Compare Models 
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")

#Best Model 
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

#Save Final Model for Later Use 
saveRDS(en, "final_model.rds") #allows you to save the model and use it later
fm <- readRDS("final_model.rds")
print(fm)

#Prediction
p1 <- predict(fm, testing)
postResample(p1, testing$medv)

p2 <- predict(fm, training)
postResample(p2, training$medv)
