#url: https://youtu.be/AVx7Wc1CQ7Y

#Getting and Fixing Data
rawdata <-  read.csv("binary.csv")
rawdata$admit <- as.factor(rawdata$admit)
rawdata$rank <- as.factor(rawdata$rank)

#Two-way table of factor variables (contingency table)
xtabs(~admit +rank, data = rawdata)

#Data Partition 
library(caret)
set.seed(100)
inTrain <- createDataPartition(rawdata$admit, 
                               p = 0.8, list = FALSE)
training <-rawdata[inTrain,]
testing <- rawdata[-inTrain,]

#Logistic Regression model (his way)
logi_model <- glm(admit ~., 
                  data = training, 
                  family = "binomial")
summary(logi_model)

#prediction 
p1 <- predict(logi_model, testing, type = "response") #gives probabilities of being admitted
head(p1)
pred1 <- ifelse(p1>0.5, 1,0)
confusionMatrix(table(Predicted = pred1, Actual = testing$admit))

#Logistic Regression (using caret)
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       savePredictions = TRUE)
lreg <- train(admit~.,
              data = training, 
              method = "glm",
              family = "binomial",
              trControl = custom)
lreg
summary(lreg)
varImp(lreg)

#prediction 
pred2 <- predict(lreg, testing)
confusionMatrix(table(Prediction = pred2, Actual = testing$admit))
