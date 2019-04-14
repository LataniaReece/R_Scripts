library(ISLR); library(ggplot2); library(caret)
data(Wage); Wage <- subset(Wage, select = -c(logwage))
summary(Wage)

#Create training and test sets 
inTrain <- createDataPartition(y=Wage$wage, 
                               p = 0.7, list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

featurePlot(x = training[,c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")

qplot(age, wage, data = training)
qplot(age, wage, color = jobclass, data = training) #You can see that those in the information jobclass dominate the 
#higher wages
qplot(age, wage, color = education, data = training)

#Fit a linear model: 
modFit <- train(wage ~ age + jobclass + education, 
                method = "lm", data = training)
finMod <- modFit$finalModel
modFit


plot(finMod,1, pch = 19, cex = 0.5)

#Color by variable not used in the model 
qplot(finMod$fitted, finMod$residuals, color = race, data = training) #Can be used to identify potential trends 


plot(finMod$residuals, pch = 19) #should not be a pattern, this is residual plotted by row
#If you see a pattern that means there is some variable unaccounted for like time or something else 

#predicted vs truth in test set 
pred <- predict(modFit, testing)
qplot(wage, pred, color = year, data = testing)

#Using all covariates 
modFitAll <- train(wage ~., data = training, method = "lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data = testing)
