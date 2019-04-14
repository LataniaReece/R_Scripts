library(ISLR); library(ggplot2); library(caret)


data(Wage)

inTrain <- createDataPartition(y=Wage$wage, 
                               p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")

qplot(age,wage,data = training)
qplot(age,wage,colour = jobclass, data=training) #most individuals with higher wages come from the information age 
qq <-qplot(age, wage, color = education, data = training)
qq+ geom_smooth(method = 'lm', formula = y~x)

#Making Factors
library(Hmisc)
cutWage <- cut2(training$wage, g=3)

p1 <- qplot(cutWage, age, data = training, fill=cutWage, 
            geom =c("boxplot"))
p2 <- qplot(cutWage, age, data = training, fill = cutWage, 
            geom=c("boxplot","jitter")) #makes sure the data points are actually on the suggested boxplot 

library(gridExtra)
grid.arrange(p1, p2, ncol = 2) 

t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1, 1) #proportion by row 
prop.table(t1, 2) #proportion by column

qplot(wage, colour = education, data = training, geom = "density")