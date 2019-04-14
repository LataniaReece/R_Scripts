library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type, 
                               p=0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="", xlab = "ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)

preObj <- preProcess(training[,-58], method = c("center", "scale")) #standardizes the predictors (58 = response variable)
trainCapAveS <- predict(preObj, training[,-58])$capitalAve

testCapAveS <- predict(preObj, testing[,-58])$capitalAve #standardizes the testing variables using the mean and sd 
#created in the training set NOT the ones in the test set

set.seed(32343)
modelFit <- train(type ~., data = training, 
                  preProcess=c("center","scale"), method = "glm")

preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

#imputing data (first gonna add missing values to the data)

set.seed(13343)

#Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05)==1
training$capve[selectNA] <- NA

table(selectNA)

#Impute and Standardize
preObj <- preProcess(training[,-58],method="bagImpute") #
library(RANN)
capAve <- predict(preObj, training[,-58])$capAve #Not working but it should 

#Standardize true values 
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
