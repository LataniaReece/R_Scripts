data(iris); library(ggplot2)
table(iris$Species)

library(caret)
#create training and test sets 
inTrain <- createDataPartition(y = iris$Species, 
                                p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

qplot(Petal.Width, Sepal.Width, color = Species, data = training )

modfit <- train(Species ~., method = "rpart", data = training) #predicting with trees
modfit$finalModel

#one interpretation: So, here for example, there's a split that says petal.length is less than 2.45
#and if that happens then you in that case all of the examples that have pedal length less than 2.45 
#belong to this bc setosa

#Visualizing the plot tree
plot(modfit$finalModel, uniform = TRUE,
     main = "Classification Tree")
text(modfit$finalModel, use.n = TRUE, all= TRUE, cex = 0.8)

#Visualizing with a prettier model 
library(rattle)
fancyRpartPlot(modfit$finalModel)

#predicting new values 
preds <- predict(modfit, newdata = testing)

#checking accuracy
t <- table(testing$Species, preds)
confusionMatrix(t)

#https://en.wikipedia.org/wiki/Sensitivity_and_specificity
#sensitivity: true positive rate, recall, probability of detection - correctly identified positives
#specificity: true negative rate - correctly identified negatives

#http://sphweb.bumc.bu.edu/otlt/MPH-Modules/EP/EP713_Screening/EP713_Screening5.html
#positive predictive value: the probability that subjects with a positive screening test truly have 
    #the disease 
#negative predictive value: prob that subjects with a negative screening test truly do not have the
    #disease


