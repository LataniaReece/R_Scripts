#PCR Website: https://www.youtube.com/watch?v=MrtPbruYbWY&feature=youtu.be
library(ISLR)
data("Hitters")
data <- Hitters

#predicting salary using pcr vs pls, will remove observations with NA as salary just for demo
complete_data <- complete.cases(data)
data <- data[complete_data,]

#Data Partition
library(caret)
set.seed(250)
inTrain <- createDataPartition(y = data$Salary, 
                               p = 0.7,
                               list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain,]

#PLS as shown in the video
library(pls)
set.seed(1)
fit <- pcr(Salary ~., 
           data = training,
           scale = TRUE,
           validation = "CV")
summary(fit)
validationplot(fit, val.type = "RMSE") #shows PCs used vs error, will keep 5                                                                -
p1 <- predict(fit, newdata = testing, 
              comps =  6)
postResample(p1, testing$Salary)
options(scipen=999)
varImp(fit)

#PLS using train
set.seed(1)
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5)
fit2 <- train(Salary~., 
             data = training, 
             method = "pls",
             preProces = c("center", "scale"),
             trControl = custom,
             tuneLength = 15)

#PCR Website: https://youtu.be/MrtPbruYbWY
#Principal Component Regression as in video:

library(pls)
set.seed(1)
fit <- pcr(Salary~., 
           data = data,
           scale = TRUE, 
           validation = "CV")
summary(fit)
validationplot(fit, val.type = "MSEP")

#PCA using the train function 
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5)

fit2 <- train(Salary~., 
              data = data, 
              method = "lm",
              preProc = c("scale", "center", "pca"),
              tuneLength = 19,
              trControl = custom)
fit2
summary(fit2)
fit2$preProcess$rotation #used to see the PCA loadings
plot(fit2$finalModel) #see residual plots 
