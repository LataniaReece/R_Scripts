library(dplyr)
library(ggplot2)

#data
rawdata <- read.csv("binary.csv")
xtabs(~admit+rank, data = rawdata) #crosstabs, frequencies should be more than 5
#rawdata$rank <- as.factor(rawdata$rank)
rawdata$admit <- as.factor(rawdata$admit)


#visualization
library(psych)
pairs.panels(rawdata[,-1])
library(RColorBrewer)
palette(brewer.pal(n = 8, name = "Set2"))

boxplot(rawdata$gre ~ rawdata$admit, col = rawdata$admit)
qplot(gre, color = admit, data = rawdata, geom = "density")

#data partition
set.seed(181)
inTrain <- createDataPartition(y = rawdata$admit,
                               p = 0.7, list = FALSE)
training <- rawdata[inTrain,]
testing <- rawdata[-inTrain,]

#Naive Bayes Model 
library(naivebayes)
fit <- naive_bayes(admit ~., data = training)
fit
plot(fit) #gives 3 plots

str(training)
#predict 
library(e1071)
pred <- predict(fit, newdata = testing, type = "class") #in order to predict, you have to have int
#predictors
confusionMatrix(table(pred, testing$admit))

#Using kernel is good when numerical variables are not normally distributed 
fit <- naive_bayes(admit ~., data = training, usekernel = TRUE)
fit
plot(fit) #gives 3 plots

str(training)
#predict 
library(e1071)
pred <- predict(fit, newdata = testing, type = "class") #in order to predict, you have to have int
#predictors
confusionMatrix(table(pred, testing$admit)) #it does a little bit better 



