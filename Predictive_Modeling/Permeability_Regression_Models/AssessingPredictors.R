#Explained on p 8 
#predicting compounds' permeability 
#Data 
#165 compounds 
# 1107 molelcular fingerprints determined - binary sequence that reps the presence or absence of 
# a specific molecular structure 
# reponse is highly skewed 
# predictors are sparse and may predictors are strongly associated

library(caret)
library(AppliedPredictiveModeling)

set.seed(0)

data(permeability)
data <- data.frame(fingerprints, permeability)