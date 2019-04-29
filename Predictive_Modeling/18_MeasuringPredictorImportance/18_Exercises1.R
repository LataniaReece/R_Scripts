#Ch 18 Exercises 

#18.1 
#Churn dataset - prdict customer churn based on their account 
#19 predictors related to customare account 

#Going to make number of customer service calls a factor variable, 

str(churnTrain)

library(C50)
data(churn)
str(churnTrain)

#Correlations between predictors 
library(dplyr)
numeric_predictors <- select_if(churnTrain, is.numeric)
correlations <- cor(numeric_predictors)

library(corrplot)
dev.off()
corrplot(correlations, order = 'hclust')

#Importance of categorical predictors 
categorical_predictors <- select_if(churnTrain, is.factor)
str(categorical_predictors)

#2 class predictors (area code, international plan, voicemail plan, churn)

#more than two class(state)
catTables <- apply(categorical_predictors[,-5], 2,
                   function(x, y) table(x, y),
                   y = categorical_predictors$churn)

fisherResults <- lapply(catTables, function (x) fisher.test(x, simulate.p.value = TRUE))
chiResults <- lapply(catTables, chisq.test)

#50 different states but 3 different area codes?
table(categorical_predictors$state)
table(categorical_predictors$area_code)

#Importance of continuous predictors 
getTstats <- function(x,y)
{
  tTest <- t.test(y ~ x)
  out <- c(tStat = tTest$statistic, p = tTest$p.value)
  out
}

tVals <- apply(numeric_predictors, 2, getTstats,
               x = churnTrain$churn)
#switch the dimensions 
tVals <- t(tVals)

#total_day_minutes > total_day_charge > number_customer_service_calls
rocValues <- filterVarImp(x = numeric_predictors,
                         y = churnTrain$churn)

rocValues[order(-rocValues$yes),]

#Relief 
numeric_predictors$churn <- churnTrain$churn
library(CORElearn)
reliefValues <- attrEval(churn ~.,
                         data = numeric_predictors,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)

reliefValues <- attrEval(churn ~.,
                         data = categorical_predictors,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)

#Balancing classes: I'm sure theres a faster way to do this

yesChurn <- churnTrain[churnTrain$churn == 'yes',]
noChurn <- churnTrain[churnTrain$churn == 'no',]

yesSample <- sample(nrow(yesChurn), size = 300)
noSample <- sample(nrow(noChurn), size = 300)

yesPeeps <- yesChurn[yesSample,]
noPeeps <- noChurn[noSample,]

balanced_data <- rbind(yesPeeps, noPeeps)

balanced_numeric_pred <- select_if(balanced_data, is.numeric)
balanced_cat_pred <- select_if(balanced_data, is.factor)

#Relief
set.seed(230)

reliefValues1 <- attrEval(churn ~.,
                         data = numeric_predictors,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)

set.seed(230)
reliefValues2 <- attrEval(churn ~.,
                         data = categorical_predictors,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)

balanced_numeric_pred$churn <- balanced_data$churn
set.seed(230)
reliefValues3 <- attrEval(churn ~.,
                         data = balanced_numeric_pred,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)

balanced_cat_pred$churn <- balanced_data$churn
set.seed(230)
reliefValues4 <- attrEval(churn ~.,
                         data = balanced_cat_pred,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)
