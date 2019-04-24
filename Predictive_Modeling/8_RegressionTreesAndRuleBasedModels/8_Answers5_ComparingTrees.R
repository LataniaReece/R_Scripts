#8.5 ans

library(caret)
library(AppliedPredictiveModeling)
library(rpart)

set.seed(0)

data(tecator) 

fat = endpoints[,2] # try to predict fat content 
absorp = data.frame(absorp)

# For various models build and then compare performance:
#
set.seed(0)
rpart_model = train( absorp, fat, method="rpart", 
                     preProcess=c("center","scale"),
                     trControl=trainControl(method="repeatedcv",repeats=5) )

set.seed(0)
rf_model = train( absorp, fat, method="rf", preProcess=c("center","scale"),
                  trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(rf_model, 'rfmodel_ans_8.5.rds')

set.seed(0)
cforest_model = train( absorp, fat, method="cforest", preProcess=c("center","scale"),
                       trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(cforest_model, 'cforestmodel_ans_8.5.rds')

gbmGrid = expand.grid( interaction.depth = seq( 1, 7, by=2 ),
                       n.trees = seq( 100, 1000, by=100 ),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10 )
set.seed(0)
gbm_model = train( absorp, fat, method="gbm", preProcess=c("center","scale"),
                   tuneGrid = gbmGrid, 
                   trControl=trainControl(method="repeatedcv",repeats=5),
                   verbose=FALSE )

saveRDS(gbm_model, 'gbmModel_ans_8.5.rds')

# EPage 82 
resamp = resamples( list(rpart=rpart_model,cforest=cforest_model,rf=rf_model,gbm=gbm_model) )
print( summary(resamp) )

dotplot( resamp, metric="RMSE" )

print( summary(diff(resamp)) )

