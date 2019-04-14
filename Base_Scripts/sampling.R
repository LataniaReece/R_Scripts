library(C50)
data(churn)

#catools sample 
library(caTools)
mysplit <- sample.split(rawdata$churn, SplitRatio = 0.7)
train <- subset(rawdata, mysplit == T)
test <- subset(rawdata, mysplit == F)

table(train$churn)
table(test$churn)

#caret sample
library(caret)
inTrain <- createDataPartition(rawdata$churn, p = 0.7, list = FALSE)
training <- rawdata[inTrain,]
testing <- rawdata[-inTrain,]
