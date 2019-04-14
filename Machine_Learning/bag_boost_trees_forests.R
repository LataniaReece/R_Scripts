#https://youtu.be/aBYC_n7DNqI

#Using admission data to demonstrate the dif techniques used in the video above 
#target variable: binary version of admit, I created 

#======GettingData=============
library(textclean); library(dplyr)
rawdata <- read.csv("Admission_Predict.csv")
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)

rawdata$admit <- ifelse(rawdata$chanceofadmit <= 0.5, "No", "Yes")
rawdata <- rawdata %>%
  dplyr::select(grescore, cgpa, universityrating, admit)
rawdata$admit <- as.factor(rawdata$admit)
rawdata$universityrating <- as.factor(rawdata$universityrating)
#======Test/Train===============
library(caret)
set.seed(10)

inTrain <- createDataPartition(y = rawdata$admit,
                               p = 0.7, list = FALSE)
training <- rawdata[inTrain,]
testing <- rawdata[-inTrain,]
#=========ClassificationTree=================
library(tree)
set.seed(10)
tree.data <- tree(admit ~., data = training)
tree.pred <- predict(tree.data, testing[,-4], type = "class")
t1 <- table(tree.pred, testing$admit)
confusionMatrix(t1)

#=======ClassificationTree w/Pruning===============
set.seed(10)
cv.data <- cv.tree(tree.data, FUN=prune.misclass)
plot(cv.data$size, cv.data$dev, type = "b") 
#dev = deviance and we want the model with the least deviance, just for demonstration, using the one 
#w/ 3 trees
prune.data <- prune.misclass(tree.data, best = 3)
tree.pred <- predict(prune.data, testing, type = "class")
t2 <- table(tree.pred, testing$admit)
confusionMatrix(t2)
plot(prune.data); text(prune.data, pretty = 0)

#=====RegressionTree============================
library(MASS)
data(Boston)
set.seed(10)
inTrain2 <- createDataPartition(y=Boston$medv, 
                                p=0.7, list = FALSE)
training2 <- Boston[inTrain2,]
testing2 <- Boston[-inTrain2,]

#model
set.seed(10)
tree.boston <- tree(medv~., data = training2)
yhat <- predict(tree.boston, newdata = testing2)
postResample(yhat, testing2$medv) #gives RMSE, Rsquared, MAE 

#pruning 
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type = "b")
prune.boston <- prune.tree(tree.boston, best = 8)
yhat <- predict(prune.boston, newdata = testing2)
postResample(yhat, testing2$medv) #in this case, the best tree was the one w/8


#========RandomForest and Bagging===============
library(randomForest)
set.seed(10)
bag.boston <- randomForest(medv~., data = training2, mtry = 13, importance = TRUE)
yhat.bag <- predict(bag.boston, newdata = testing2)
postResample(yhat.bag, testing2$medv)
varImpPlot(bag.boston) #shows which variables were more important in determining trees

#Boosting 
library(gbm)
set.seed(10)
boost.boston <- gbm(medv ~., data = training2, distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)
#gaussian = regression, bernoulli = classification 
yhat.boost <- predict(boost.boston, newdata = testing2, n.trees = 5000)
postResample(yhat.boost, testing2$medv)

boost.boston <- gbm(medv~., data = training2, distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2)
yhat.boost <- predict(boost.boston, newdata = testing2, n.trees = 5000)
postResample(yhat.boost, testing2$medv)
