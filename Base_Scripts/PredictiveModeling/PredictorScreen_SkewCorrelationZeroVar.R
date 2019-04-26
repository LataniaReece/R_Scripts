library(caret)
library(AppliedPredictiveModeling)
data(permeability)

data <- data.frame(fingerprints, permeability)

# Look for degenerate columns: 
grep('permeability', names(data))
zero_cols = nearZeroVar(data[,-1108])
length(colnames(data)[zero_cols])
data <- data[,-zero_cols]

#Correlation between predictors 
highCorr = findCorrelation(cor(data), cutoff=0.80)
data_independent = data[,-highCorr]

#skewness 
library(e1071)
skew_values <- round(apply(data_independent, 2, skewness), 2)
skew_values <- data.frame(variables = names(skew_values), value = skew_values, 
                          row.names = 1:length(skew_values))
highskew <- skew_values[abs(skew_values$value) > 1.5,]
head(highskew[order(-abs(highskew$value)),], n = 15)