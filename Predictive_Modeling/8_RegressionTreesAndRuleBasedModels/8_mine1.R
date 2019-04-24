library(mlbench)
set.seed(200)
simulated <- mlbench.friedman1(200, sd = 1)
simulated <-cbind(simulated$x, simulated$y)
simulated <-as.data.frame(simulated)
colnames(simulated)[ncol(simulated)] <- 'y'

#a
#Fit a random forest model and estimate variable importance 
library(randomForest)
library(caret)
model1 <- randomForest(y ~., data = simulated,
                       importance = TRUE,
                       ntree = 1000)
rfIMP1 <- varImp(model1, scale = FALSE)

#Adding additional predictor that is highly correlated with on of informative predictors 
simulated$duplicate1 <- simulated$V1 + rnorm(100) *.1
cor(simulated$duplicate1, simulated$V1)

model2 <- randomForest(y ~., data = simulated,
                       importance = TRUE,
                       ntree = 1000)
rfIMP2 <- varImp(model2, scale = FALSE)

#adding another one that is highly corrlated with V1
simulated$duplicate2 <- simulated$V1 + rnorm(100) *.05
cor(simulated$duplicate2, simulated$V1)

model3 <- randomForest(y ~., data = simulated,
                       importance = TRUE,
                       ntree = 1000)
rfIMP3 <- varImp(model3, scale = FALSE)

#Adding correlated predictors reduces the importance of V1 

#c
#use cforest to make conditional tree and calculate predictor importance 
library(party)
simulated$duplicate1<- NULL
simulated$duplicate2<- NULL

cforest_model <- cforest(y~., data = simulated)
varImp(cforest_model)

#d see variable importance for other type of trees 
#M5
library(RWeka)
m5Tree <- M5P(y~., data = simulated)

#Bagged 
library(ipred)
baggedTree <- bagging(y~., data = simulated)
varImp(baggedTree)

#Boosted - var imp didn't work
library(gbm)
gbmModel <- gbm(y~., data = simulated, distribution = 'gaussian')
varImp(gbmModel) # didn't work
summary.gbm(gbmModel) #also gives variable importance 

#Cubist 
library(Cubist)
grep('y', names(simulated))
cubistMod <- cubist(simulated[,-11], simulated[,11])
varImp(cubistMod)
