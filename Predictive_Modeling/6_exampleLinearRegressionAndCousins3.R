library(AppliedPredictiveModeling)
data("ChemicalManufacturingProcess")
data <- ChemicalManufacturingProcess
#57 predictors - 12 = input biological material, 45 = process predictors,
#outcome = Yield

#checking missing data 
percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(data, 1, percent_miss)
colmiss <- apply(data, 2, percent_miss)

miss_rows <- subset(data, rowmiss > 0)
col_miss <- data[colmiss > 0]

nrow(miss_rows)
length(col_miss)

#impute missing data 
library(mice)
grep('Yield', names(data))
impute <- mice(data[,-1], m=3, seed = 124)
saveRDS(impute, "imputeChem.rds")

#Distribution of observed/imputed values
stripplot(impute)

#Complete Data 
complete_data <- complete(impute, 2)
complete_data <- cbind(complete_data, Yield = data$Yield)

#Data Partition 
library(caret)
set.seed(10)
inTrain <- createDataPartition(complete_data$Yield,
                               p = 0.7,
                               list = FALSE)
training <- complete_data[inTrain,]
testing <- complete_data[-inTrain,]

#PLS
ctrl = trainControl(method = 'repeatedcv',
                    number = 10,
                    repeats = 3)
set.seed(10)
plsFit <- train(Yield ~.,
                data = training, 
                method = 'pls',
                preProcess = c('center', 'scale'),
                tuneLength = 20,
                trControl = ctrl)
plsFit
plot(plsFit)

#predict 
pred <- predict(plsFit, testing)
postResample(pred, testing$Yield)

#Variable importance
imp <- varImp(plsFit)$importance
imp <- data.frame(variables   = rownames(imp),
                  overall = imp$Overall)
imp[order(imp$overall,decreasing = T),]

#Assess the relationship between top 11 predictors and response 
imp$variables <- as.character(imp$variables)
top_pred <- imp[imp$overall >= 60,]
top_pred <- top_pred$variables

library(dplyr)
top_pred_data <- complete_data %>%
  select(top_pred, Yield)

correlations <- cor(top_pred_data)
correlations
library(corrplot)
corrplot(correlations, method="circle", mar=c(1,1,1,1))
