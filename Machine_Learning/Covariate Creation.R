library(kernlab); data(spam)

#Covariate  = the numerous vairable used to predict the response variable 

spam$capitalAveSq <- spam$capitalAve^2 #you can create a covariate from an already existing one 

#Another example of covariate creation
library(ISLR); library(caret); data(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

dummies <- dummyVars(wage ~ jobclass, data = training) #creating dummy variables, you could imagine 
#that a variable with many factors, this would be helpful
head(predict(dummies, newdata = training))

#Removing Zero Covariates (meaning that they don't have enough variability to be predictive)
nsv <- nearZeroVar(training, saveMetrics = TRUE)


#create polynomial variables 
library(splines)
bsBasis <- bs(training$age, df = 3) #creates age, age^2 and age ^3
head(bsBasis)

#Fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), col = "red", pch = 19, cex = 0.5)

#Splines on the test set
predict(bsBasis, age = testing$age) #creates the covariates on the test data using the same procedure used
#on the training set
