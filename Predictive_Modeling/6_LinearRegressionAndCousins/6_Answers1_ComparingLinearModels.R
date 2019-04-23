#6.1


library(caret) 

set.seed(0)
data(tecator) 
#absorp = predictors 
#endpoints = columns of different responses

# Part (b):
#
pr = prcomp( absorp )

vars = pr$sdev^2
total_var = sum(vars)
plot( ( vars/total_var ) * 100, xlim=c(0,40), type='b', pch=16, xlab='number of components', ylab='percent of total variation' )
grid()

# Part (c):
# 
fat = endpoints[,2]
absorp = data.frame(absorp)

# For various models build and then compare performance:
#
set.seed(0)
lm_model = train( absorp, fat, 
                  method="lm", 
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="repeatedcv",repeats=5) )

# For rlm we cannot have a singular predictor covariance matrix thus we preprocess with PCA:
# 
set.seed(0)
rlm_model = train( absorp, fat, 
                   method="rlm",
                   preProcess = c('pca'),
                   trControl=trainControl(method="repeatedcv",repeats=5) )

set.seed(0)
pls_model = train( absorp, fat, method="pls",
                   # the default tuning grid evaluates comonents 1 ... tuneLength
                   tuneLength=40, 
                   preProcess=c("center","scale"), trControl=trainControl(method="repeatedcv",repeats=5) )

# Ridge regression training:
#
ridgeGrid = data.frame(.lambda=seq(0,1,length=20))
set.seed(0)
ridge_model = train( absorp, fat, method="ridge",
                     tuneGrid = ridgeGrid,
                     preProcess=c("center","scale"),
                     trControl=trainControl(method="repeatedcv",repeats=5) )

saveRDS(ridge_model, 'ridgemodel_ans_6.2.rds')
# Elastic net training:
# 
enetGrid = expand.grid(.lambda=seq(0,1,length=20), .fraction=seq(0.05, 1.0, length=20))
set.seed(0)
enet_model = train( absorp, fat, method="enet",
                    tuneGrid = enetGrid,
                    preProcess=c("center","scale"),
                    trControl=trainControl(method="repeatedcv",repeats=5) )
saveRDS(enet_model, 'enetmodel_ans_6.2.rds')

# EPage 82 
resamp = resamples( list(lm=lm_model,rlm=rlm_model,pls=pls_model,enet=enet_model) )
print( summary(resamp) )
dotplot( resamp, metric="RMSE" )
print( summary(diff(resamp)) )

