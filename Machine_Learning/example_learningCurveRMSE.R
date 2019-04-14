#url : https://stackoverflow.com/questions/20370827/plot-learning-curves-with-caret-package-and-r/40885119#40885119

# set seed for reproducibility
set.seed(7)

# randomize mtcars
mtcars <- mtcars[sample(nrow(mtcars)),]

# split iris data into training and test sets
library(caret)
mtcarsIndex <- createDataPartition(mtcars$mpg, p = .625, list = F)
mtcarsTrain <- mtcars[mtcarsIndex,]
mtcarsTest <- mtcars[-mtcarsIndex,]

# create empty data frame 
learnCurve <- data.frame(m = integer(21),
                         trainRMSE = integer(21),
                         cvRMSE = integer(21))

# test data response feature
testY <- mtcarsTest$mpg

# Run algorithms using 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"

# loop over training examples
for (i in 3:21) {
  learnCurve$m[i] <- i
  
  # train learning algorithm with size i
  fit.lm <- train(mpg~., data=mtcarsTrain[1:i,], method="lm", metric=metric,
                  preProc=c("center", "scale"), trControl=trainControl)        
  learnCurve$trainRMSE[i] <- fit.lm$results$RMSE
  
  # use trained parameters to predict on test data
  prediction <- predict(fit.lm, newdata = mtcarsTest[,-1])
  rmse <- postResample(prediction, testY)
  learnCurve$cvRMSE[i] <- rmse[1]
}

# plot learning curves of training set size vs. error measure
# for training set and test set
plot(log(learnCurve$trainRMSE),type = "o",col = "red", xlab = "Training set size",
     ylab = "Error (RMSE)", main = "Linear Model Learning Curve")
lines(log(learnCurve$cvRMSE), type = "o", col = "blue")
legend('bottom', c("Train error", "Test error"), lty = c(1,1), lwd = c(1, 1),
       col = c("red", "blue"))

head(learnCurve)
