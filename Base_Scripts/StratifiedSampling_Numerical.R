library(AppliedPredictiveModeling)
data("permeability")
fingerprints <- data.frame(fingerprints)
permeability <- data.frame(permeability)
#fingerprints = predictors, permeability = response

#How to split this data, uneven numerical column?

#First look at distirbution of response
hist(permeability)

#Based on lower values having more observations, will create low medium 
#and high groups and then split them - p.68
library(dplyr)
response_data <- data.frame(permeability = permeability)
response_data <- data %>%
  mutate(groups = cut(permeability, breaks=c(0,20,40,60), labels = c('low', 'medium', 'high')))

#checking that it worked
table(response_data$groups)
nrow(response_data[permeability < 20,])
nrow(response_data[permeability > 20 & permeability < 40,])
nrow(response_data[permeability > 40,])

#Data Partition 
library(caret)
set.seed(1)
inTrain <- createDataPartition(response_data$groups,
                               p = 0.7, 
                               list = FALSE)
#training set
training_preds <- fingerprints[inTrain,]
training_resp <- response_data[inTrain,]
#testing set
testing_preds <- fingerprints[-inTrain,]
testing_resp <- response_data[-inTrain,]
table(training_resp$groups)
table(testing_resp$groups)
