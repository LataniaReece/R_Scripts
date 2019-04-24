l#8.3 ans : https://rstudio-pubs-static.s3.amazonaws.com/377753_4d8f00c61d114ffc885e7a262beaa008.html
#What does increasing interacton depth do on predcitor importance and RMSE 

#A: an increase in interaction depth will increase the number of predictors and RMSE, variable importance 
# will spread across more predictors and the RMSE-iteraions graphe will ahve a higher interacept that increases 
# the negative slope

#Models with a lower learning rate see greater improvements from interaction depth than .
#models with higher learning rate

library(caret)
library(AppliedPredictiveModeling)
data(solubility)

library(gbm)
set.seed(10)
#original gbm, shrinkage = 0.001, bag fraction = 0.5
gbmModel <- gbm.fit(solTrainXtrans, solTrainY, distribution = 'gaussian')
head(summary.gbm(gbmModel, plot = FALSE), n = 20)

#interaction depth of 2
gbmModel2 <- gbm.fit(solTrainXtrans, solTrainY, distribution = 'gaussian', interaction.depth = 2)
head(summary.gbm(gbmModel2, plot = FALSE), n = 20)

#learning rate (shrinkage) ----------- 
#(https://www.analyticsvidhya.com/blog/2016/02/complete-guide-parameter-tuning-gradient-boosting-gbm-python/)
# GBM works by starting with an initial estimate which is updated using the output of each tree,
#the learning parameter controls the magnitude of this change in the estimates.

#Lower values are generally preferred as they make the model robust to the specific characteristics of tree 
#and thus allowing it to generalize well.

# Lower values would require higher number of trees to model all the relations and 
#will be computationally expensive.

#increase shrinkage, increases learning rate, spreads the variable importance
set.seed(10)
gbmModel2 <- gbm.fit(solTrainXtrans, solTrainY, distribution = 'gaussian', shrinkage = 0.1)
head(summary.gbm(gbmModel2, plot = FALSE), n = 20)

#increase bag fraction - less predictors are important 
gbmModel2 <- gbm.fit(solTrainXtrans, solTrainY, distribution = 'gaussian', bag.fraction = 0.8)
head(summary.gbm(gbmModel2, plot = FALSE), n = 20)



