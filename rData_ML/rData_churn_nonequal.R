library(C50)
data(churn)

#Best algorithm for predicting churn
rawdata <- rbind(churnTest, churnTrain)
anyNA(rawdata) #no missing data 

#Data Partition
library(caret)
set.seed(100)
inTrain <- createDataPartition(rawdata$churn,
                               p = 0.6, list = FALSE)
training <- rawdata[inTrain,]
valid <- rawdata[-inTrain,]
set.seed(100)
inValid <- createDataPartition(valid$churn,
                               p=0.5, list = FALSE)
cv_data <- valid[inValid,]
testing <- valid[-inValid,]

#Quick and Dirty Algorithm, decision trees 
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)
#information gain
set.seed(100)
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
set.seed(100)
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

#Comparing models 
tree_models <- resamples(list(info_tree = tree_fit,
             gini_tree = tree_fit2))
summary(tree_models)

#Random Forests
library(caret)
set.seed(100)
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3)
tunegrid <- expand.grid(.mtry=3)
rforest <- train(churn~.,
                 data = training,
                 method = "rf",
                 tuneGrid = tunegrid,
                 ntree = 500,
                 trControl = control)

pred_forest <- predict(rforest, cv_data)
confusionMatrix(table(pred_forest, cv_data$churn))
#logistic regression - are the variables at the top of the tree actually important 
#also assessing predictive value 
set.seed(100)
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
fit2 <- naive_bayes(churn ~.,
                   data = training,
                   usekernel = TRUE)
fit
plot(fit)
#prediction 
pred <- predict(fit, 
                newdata = cv_data,
                type = "class")
confusionMatrix(table(Predicted = pred, Actual = cv_data$churn))

#Naive bayes in train
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)
fit <- train(churn ~.,
             method = "naive_bayes",
             data = training,
             trControl = custom,
             tunegrid = c(usekernel = TRUE))

fit

modelLookup(model = "naive_bayes")
?naive_bayes
fit
plot(fit)
#Comparing Models 
resamps <- resamples(list(tree_info = tree_fit,
                          tree_gini = tree_fit2,
                          log_reg = lreg,
                          nb = fit,
                          nb_kernel = fit2))

class(fit2)
