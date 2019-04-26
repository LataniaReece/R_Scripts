library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)
names(solTrainXtrans)


#Screening Predictors - starting with 228 predictors----------------
#SolTrainX
#1st. since fingerprint predictors are categorical and binary, preprocessing won't do much
#selecting continuous predictors 

library(dplyr)
continuous_predictors <- solTrainX %>%
  select(-(starts_with('F')))

#Correlations of predictors 
correlations <- cor(continuous_predictors)
library(corrplot)
corrplot(correlations, order = 'hclust')

#PCA analysis can also tell you about correlations. Can the data be summed in smaller dimensions?
pca <- prcomp(continuous_predictors, 
              center = TRUE, scale = TRUE)
pcaVar <- round(pca$sdev^2, 4)
pcaVar_percent <- round(pcaVar/sum(pcaVar)* 100, 1)
pcaVar_percent_cum <- cumsum(pcaVar_percent)

#Scree Plot 
plot(1:10, pcaVar_percent[1:10], 
     type = 'b',
     main = 'Scree Plot',
     xlab = 'Components',
     ylab = 'Variance')
axis(side = 1, at = (1:10))
axis(side = 2, at = seq(0, 100, by = 5))

#Skewness 
library(e1071)
skew_values <- round(apply(continuous_predictors, 2, skewness), 2)
skew_values <- data.frame(variables = names(skew_values), value = skew_values, 
                          row.names = 1:length(skew_values))
highskew <- skew_values[abs(skew_values$value) > 1.5,]
head(highskew[order(-abs(highskew$value)),], n = 15)

#Applying BoxCox - make sure to add a constant or it will just ignore negative and zero values

library(caret)
set.seed(0)
boxcox <- preProcess(continuous_predictors + 1, method = 'BoxCox')
continuous_predictorsTrans <- predict(boxcox, continuous_predictors + 1)


#Categorical Predictors (208 of them)
#imabalances?
categorical_predictors <- solTrainX %>%
  select(starts_with('F'))

#Frequency Distribution into a df
freqDist <- apply(categorical_predictors, 2, function(x) prop.table(table(x)))
freqDist <- round(freqDist,2)

df_freqDist <- data.frame(t(check))
imbalancedClasses <- df_freqDist[df_freqDist$X0 <= 0.2 | df_freqDist$X1 <= 0.2,]
#140 of them are imbalanced 