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
data("permeability")
set.seed(0)
fingerprints <- data.frame(fingerprints)
permeability <- data.frame(permeability)
data <- cbind(fingerprints, permeability)

# Look for degenerate columns: 
zero_cols <- nearZeroVar(data[,-1108])
colnames(data)[zero_cols]
zeroVar <- fingerprints[,zero_cols]
freqDist_zeroVar <- apply(zeroVar, 2, function(x) round(prop.table(table(x)), 2))
head(freqDist_zeroVar)

nozerovar_imbalanced <- imbalancedClasses[!rownames(imbalancedClasses) %in% zero_cols,]
zerovar_imbalanced <- imbalancedClasses[rownames(imbalancedClasses) %in% zero_cols,]
zerovar_notimbalanced 
  
  #Question : are all zero variance vriables imabalanced? What would make a non imbalanced variable become zero variance?