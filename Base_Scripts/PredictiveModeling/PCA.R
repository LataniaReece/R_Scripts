#Loading data 
library(caret)
library(AppliedPredictiveModeling)
set.seed(0)
data(solubility)
names(solTrainXtrans)

#selecting continuous predictors 
library(dplyr)
continuous_predictors <- solTrainX %>%
  select(-(starts_with('F')))

#PCA
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