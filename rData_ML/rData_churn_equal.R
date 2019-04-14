library(C50)
data(churn)

#Best algorithm for predicting churn
rawdata <- rbind(churnTest, churnTrain)
anyNA(rawdata) #no missing data 

#Making Churn Equal 
library(dplyr)
set.seed(300)
rawdata <- rawdata %>%
  group_by(churn) %>%
  sample_n(706)

#Data Partition - just trying this out even if not enough in training
set.seed(300)
inTrain <- createDataPartition(rawdata$churn, 
                               p = 0.6, list = FALSE)
training <- rawdata[inTrain,]
valid <- rawdata[-inTrain,]

inValid <- createDataPartition(valid$churn, 
                               p = 0.5, list = FALSE)
cv_data <- valid[inValid,]
testing <- valid[-inValid,]
#=================Building Models===========================
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)
#information gain
set.seed(333)
tree_fit <- train(churn ~.,
                  data = training,
                  method = "rpart",
                  parms = list(split = "information"), 
                  trControl = custom,
                  tuneLength = 10)

tree_fit
library(rpart.plot)
prp(tree_fit$finalModel, box.palette = "Blues", tweak = 1.2)

#prediction(info gain)
cv_pred <- predict(tree_fit, cv_data)
confusionMatrix(table(cv_pred, cv_data$churn))

#gini 
set.seed(333)
tree_fit2 <- train(churn ~.,
                   data = training,
                   method = "rpart",
                   parms = list(split = "gini"), 
                   trControl = custom,
                   tuneLength = 10)
tree_fit2
prp(tree_fit2$finalModel, box.palette = "Purples", tweak = 1.2)

#prediction(gini)
cv_pred2 <- predict(tree_fit2, cv_data)
confusionMatrix(table(cv_pred2, cv_data$churn))

#logistic regression - are the variables at the top of the tree actually important 
#also assessing predictive value 
set.seed(333)
lreg <- train(churn ~.,
              data = training, 
              method = "glm",
              family = "binomial",
              trControl = custom)
lreg
summary(lreg)

boxplot(training$total_day_minutes ~ training$churn)
cv_pred3 <- predict(lreg, cv_data)
confusionMatrix(table(Predicted = cv_pred3, Actual = cv_data$churn))

#Naive Bayes Model 
library(naivebayes)
set.seed(333)
fit <- naive_bayes(churn ~.,
                   data = training)
fit
plot(fit) #shows all variables 

#prediction
pred <- predict(fit,
                newdata = cv_data,
                type = "class") 
confusionMatrix(table(Predicted = pred, Actual = cv_data$churn))

#Naive Bayes Model with kernel
set.seed(333)
fit <- naive_bayes(churn ~.,
                   data = training,
                   usekernel = TRUE)
fit
plot(fit)
#prediction 
pred <- predict(fit, 
                newdata = cv_data,
                type = "class")
confusionMatrix(table(Predicted = pred, Actual = cv_data$churn))