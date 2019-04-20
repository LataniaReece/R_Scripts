library(caret)
data("GermanCredit")

set.seed(10)
trainIndex <- createDataPartition(y = GermanCredit$Class,
                                  p = 0.8,
                                  list= FALSE)
training <- GermanCredit[trainIndex,]
testing <- GermanCredit[-trainIndex,]

#svm train
set.seed(10)
svmFit <- train(Class ~.,
                data = training, 
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats= 5,
                                         classProbs = TRUE))
#Using the option tuneLength = 10, the cost values 2^2 thru 2^7 are evaluated.
svmFit
#line plot of average performance 
plot(svmFit)
plot(svmFit, scales = list(x = list(log = 2))) #was in the book on p86

#predictions
pred <- predict(svmFit, testing)
pred_probs <- predict(svmFit, newdata = testing, type = "prob") 
#classprobs = TRUE must be in traincontrol for this to work

#logistic regression train 
set.seed(10)
logisticReg <- train(Class ~.,
                     data = training, 
                     method = 'glm',
                     trControl = trainControl(method = 'repeatedcv',
                                              repeats = 5))
logisticReg

#comparing models 
resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
summary(resamp)
xyplot(resamp) #shows the visual relationship between SVM and Logistic 
#in this case there is a correlation

#see their differences 
modelDiff <- diff(resamp)
summary(modelDiff) #doesn't look like it does in the book p 89

