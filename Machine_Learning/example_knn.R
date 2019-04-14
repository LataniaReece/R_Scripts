#======GettingData=============
library(textclean); library(dplyr)
rawdata <- read.csv("Admission_Predict.csv")
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)
rawdata$admit <- ifelse(rawdata$chanceofadmit >= 0.73, "Yes", "No") 
#changed the threshold because there was not enough No's
rawdata <- rawdata %>%
  dplyr::select(grescore, cgpa, admit, universityrating)
rawdata$admit <- as.factor(rawdata$admit)
rawdata$grescore <- as.numeric(rawdata$grescore)
str(rawdata$admit)
#======Test/Train===============
library(caret)
set.seed(222)

inTrain <- createDataPartition(y = rawdata$admit,
                               p = 0.7, list = FALSE)
training <- rawdata[inTrain,]
testing <- rawdata[-inTrain,]
#======KNNModel==================
library(pROC)
#===classification knn, accuracy used for choosing optimal model==========
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)
fit <- train(admit~., 
             data = training, 
             method = "knn",
             tuneLength = 20,
             trControl= trControl,
             preProc = c("center", "scale"))
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = testing)
confusionMatrix(table(pred, testing$admit))

#=====classification knn, ROC used for choosing optimal model=====
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)
set.seed(222)
fit <- train(admit ~.-universityrating,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"),
             metric = "ROC",
             tuneGrid = expand.grid(k = 1:60))
fit
plot(fit) #shows accuracy based on how many neighbors were nearby 
varImp(fit) #none of the cat variables can be factor variables in order for this to work
pred <- predict(fit, newdata = testing)
confusionMatrix(table(pred, testing$admit))

#====regression knn===============================
library(mlbench)
data(BostonHousing)
data <- BostonHousing
#predicting medv using knn

set.seed(333)
inTrain <- createDataPartition(y = data$medv,
                               p = 0.7, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#using RMSE for choosing optimal
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats =3)
set.seed(333)
fit <- train(medv ~.,
             data = training, 
             tuneGrid = expand.grid(k=1:70),
             method = 'knn',
             trControl = trControl,
             preProc = c('center', 'scale'))
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = testing)
RMSE(pred, testing$medv)

#using Rsquare to choose optimal
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats =3)
set.seed(333)
fit <- train(medv ~.,
             data = training, 
             tuneGrid = expand.grid(k=1:70),
             method = 'knn',
             metric = "Rsquared",
             trControl = trControl,
             preProc = c('center', 'scale'))
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = testing)
RMSE(pred, testing$medv)

