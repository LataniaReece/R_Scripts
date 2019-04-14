#url: http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.names

#================================Data==================================================
variables <- c("ID", "clump.thickness", "uniformity.cell.size","uniformity.cell.shape",
               "marginal.adhesion","epithelial.cell.size","bare.nuclei","bland.chromatin",
               "normal.nucleoli","mitoses","diagnosis")
rawdata <- read.csv(url("http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"),
                    col.names = variables)
rawdata$diagnosis <- factor(rawdata$diagnosis, levels = c(2,4),
                            labels = c("benign", "malignant"))
rawdata$bare.nuclei <- factor(rawdata$bare.nuclei, 
                              levels = c("?", c(1:10)),
                              labels = c("NA", c(1:10)))
library(varhandle)
rawdata$bare.nuclei <- as.numeric(unfactor(rawdata$bare.nuclei))
rawdata$ID <- NULL
#========================missing_data - no missing data===================================
completeIndex <- complete.cases(rawdata)
missing_data <- rawdata[!completeIndex,]
rawdata$missing <- ifelse(rownames(rawdata) %in% rownames(missing_data),1, 0)

library(dplyr)
check <- rawdata %>%
  select(-bare.nuclei)
logi_model <- glm(missing ~., 
                  data = check, 
                  family = "binomial",
                  maxit = 1000)
summary(logi_model)

table(check$missing, check$diagnosis)
boxplot(check$uniformity.cell.shape ~ check$missing)

#removing missing data 
data <- rawdata[completeIndex,] %>%
  select(-missing)
#==========================Data Partition=================================================
library(caret)
set.seed(2)

trainIndex <- createDataPartition(data$diagnosis,
                                  p =0.6, list = FALSE)
training<- data[trainIndex,]
valid <- data[-trainIndex,]

validIndex <- createDataPartition(valid$diagnosis,
                                  p=0.5, list = FALSE)
cv_data <- valid[validIndex,]
testing <- valid[-validIndex,]

table(training$diagnosis)
#========================Decision Trees====================================================
control <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)
set.seed(10)
tree_fit <- train(diagnosis ~.,
                  data = training,
                  method = "rpart",
                  trControl = control,
                  parms = list(split = "information"),
                  tuneGrid = expand.grid(cp = seq(0,1,0.1)))
tree_fit
summary(tree_fit$finalModel)

#Visuals 
library(rattle)
fancyRpartPlot(tree_fit$finalModel)
library(rpart.plot)
rpart.plot(tree_fit$finalModel)

#Predictions 
p1 <- predict(tree_fit, cv_data, response = "prob")
confusionMatrix(table(Predicted = p1, Actual = cv_data$diagnosis), positive = "malignant")

#===========================Naive Bayes===================================================
set.seed(10)
naive_fit <- train(diagnosis ~.,
                  data = training,
                  method = "naive_bayes",
                  trControl = control)
p2 <- predict(naive_fit, cv_data)
confusionMatrix(table(Predicted = p2, Actual = cv_data$diagnosis), positive = "malignant")

#=========================Logistic Regression=============================================
set.seed(10)
lreg <- train(diagnosis~.,
              data = training, 
              method = "glm",
              family = "binomial",
              trControl = control,
              maxit = 100)
lreg
summary(lreg)
p3 <- predict(lreg, cv_data)
confusionMatrix(table(Predicted = p3, Actual = cv_data$diagnosis), positive = "malignant")
#=======================SVM=============================================================
set.seed(10)
grid_radial <- expand.grid(sigma = seq(0,1,0.1),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1))
fit_svm <- train(diagnosis~.,
                 data = training, 
                 method = "svmRadial",
                 preProcess = c("center", "scale"),
                 trControl = control,
                 tuneLength = 10,
                 tuneGrid = grid_radial)
fit_svm
p4 <- predict(fit_svm, cv_data)
confusionMatrix(table(Predicted = p4, Actual = cv_data$diagnosis), positive = "malignant")
