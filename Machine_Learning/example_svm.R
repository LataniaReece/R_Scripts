#url: https://youtu.be/pS5gXENd3a4

#Data
data(iris)
library(ggplot2)
qplot(Petal.Length, Petal.Width, data = iris,
      color = Species)

#Support Vector machine (his way) - radial (gaussian) kernel by default
library(e1071)
set.seed(10)
mymodel <- svm(Species~.,
                data = iris)
summary(mymodel)
plot(mymodel, data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
#Confusion Matrix and Misclassification Error 
pred <- predict(mymodel, iris)
library(caret)
confusionMatrix(table(Predicted = pred, Actual = iris$Species))

#SVM with linear kernel 
set.seed(10)
lin_svmmodel <- svm(Species~.,
               data = iris,
               kernel = "linear")
summary(lin_svmmodel)
pred2 <- predict(lin_svmmodel, iris)
confusionMatrix(table(Predicted = pred2, Actual = iris$Species))

#tuning (episilon & cost makes sure that they pick the best parameters and do not settle)
set.seed(10)
tmodel <- tune(svm, Species ~., data = iris, 
               ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
plot(tmodel) 
#darker areas = better performance so lower values of cost = better performance
set.seed(10)
tmodel <- tune(svm, Species ~., data = iris, 
               ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:7)))
plot(tmodel)
summary(tmodel) 
#best model 
mymodel <- tmodel$best.model
summary(mymodel)
#confusion Matrix
pred <- predict(mymodel, iris)
confusionMatrix(table(Predicted = pred, Actual = iris$Species))

#=======Caret SVM================
library(caret)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
grid_radial <- expand.grid(sigma = seq(0,1,0.1),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
custom = trainControl(method = "repeatedcv",
                   number = 10,
                   repeats = 3,
                   classProbs = TRUE)
#radialSVM
set.seed(10)
fit_svm <- train(Species~.,
                 data = iris, 
                 method = "svmRadial",
                 preProcess = c("center", "scale"),
                 trControl = custom,
                 tuneLength = 10,
                 tuneGrid = grid_radial)
fit_svm
p1 <- predict(fit_svm, iris)
confusionMatrix(p1, iris$Species)

#w/linear kernel
set.seed(10)
fit_svm2 <- train(Species~.,
                 data = iris, 
                 method = "svmLinear",
                 preProcess = c("center", "scale"),
                 trControl = custom,
                 tuneLength = 10,
                 tuneGrid = grid)
fit_svm2
p2 <- predict(fit_svm2, iris)
confusionMatrix(p2, iris$Species)
plot(fit_svm2)