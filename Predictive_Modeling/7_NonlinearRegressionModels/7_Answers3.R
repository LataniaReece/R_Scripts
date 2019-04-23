#7.3 Answers 
library(caret) 

set.seed(0)

data(tecator) 

fat = endpoints[,2] # what we want to predict 
absorp = data.frame(absorp)

zero_cols = nearZeroVar( absorp ) # look for highly correlated predictors ... none found ... 
zero_cols
# Split this data into training and testing sets:
#
training = createDataPartition( fat, p=0.8 )

absorp_training = absorp[training$Resample1,]
fat_training = fat[training$Resample1]

absorp_testing = absorp[-training$Resample1,]
fat_testing = fat[-training$Resample1]

# Build various nonlinear models and then compare performance:
# 
# Note we use the default "trainControl" of bootstrap evaluations for each of the models below: 
#
preProc_Arguments = c("center","scale")
#preProc_Arguments = c("center","scale","pca")

# A K-NN model:
# 
set.seed(0)
knnModel = train(x=absorp_training, y=fat_training, method="knn", preProc=preProc_Arguments, tuneLength=10)

# predict on training/testing sets
knnPred = predict(knnModel, newdata=absorp_training)
knnPR = postResample(pred=knnPred, obs=fat_training)
rmses_training = c(knnPR[1])
r2s_training = c(knnPR[2])
methods = c("KNN")

knnPred = predict(knnModel, newdata=absorp_testing)
knnPR = postResample(pred=knnPred, obs=fat_testing)
rmses_testing = c(knnPR[1])
r2s_testing = c(knnPR[2])


# A Neural Network model:
#
set.seed(0)
nnetModel = train(x=absorp_training, y=fat_training, method="nnet", preProc=preProc_Arguments,
                  linout=TRUE,trace=FALSE,MaxNWts=10 * (ncol(absorp_training)+1) + 10 + 1, maxit=500)

saveRDS(nnetModel, 'NN_ans_7.3.rds')
nnetPred = predict(nnetModel, newdata=absorp_training)
nnetPR = postResample(pred=nnetPred, obs=fat_training)
rmses_training = c(rmses_training,nnetPR[1])
r2s_training = c(r2s_training,nnetPR[2])
methods = c(methods,"NN")

nnetPred = predict(nnetModel, newdata=absorp_testing)
nnetPR = postResample(pred=nnetPred, obs=fat_testing)
rmses_testing = c(rmses_testing,nnetPR[1])
r2s_testing = c(r2s_testing,nnetPR[2])


# Averaged Neural Network models:
#
set.seed(0)
avNNetModel = train(x=absorp_training, y=fat_training, method="avNNet", preProc=preProc_Arguments,
                    linout=TRUE,trace=FALSE,MaxNWts=10 * (ncol(absorp_training)+1) + 10 + 1, maxit=500)

saveRDS(avNNetModel, 'avNN_ans_7.3.rds')
avNNetPred = predict(avNNetModel, newdata=absorp_training)
avNNetPR = postResample(pred=avNNetPred, obs=fat_training)
rmses_training = c(rmses_training,avNNetPR[1])
r2s_training = c(r2s_training,avNNetPR[2])
methods = c(methods,"AvgNN")

avNNetPred = predict(avNNetModel, newdata=absorp_testing)
avNNetPR = postResample(pred=avNNetPred, obs=fat_testing)
rmses_testing = c(rmses_testing,avNNetPR[1])
r2s_testing = c(r2s_testing,avNNetPR[2])


# MARS model:
#
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
set.seed(0)
marsModel = train(x=absorp_training, y=fat_training, method="earth", 
                  preProc=preProc_Arguments, tuneGrid=marsGrid)
saveRDS(marsModel, 'marsModel_ans_7.3.rds')

marsPred = predict(marsModel, newdata=absorp_training)
marsPR = postResample(pred=marsPred, obs=fat_training)
rmses_training = c(rmses_training,marsPR[1])
r2s_training = c(r2s_training,marsPR[2])
methods = c(methods,"MARS")

marsPred = predict(marsModel, newdata=absorp_testing)
marsPR = postResample(pred=marsPred, obs=fat_testing)
rmses_testing = c(rmses_testing,marsPR[1])
r2s_testing = c(r2s_testing,marsPR[2])

# Lets see what variables are most important in the MARS model: 
varImp(marsModel)

# A Support Vector Machine (SVM):
#
set.seed(0)
svmModel = train(x=absorp_training, y=fat_training, method="svmRadial", preProc=preProc_Arguments,
                 tuneLength=20)

svmPred = predict(svmModel, newdata=absorp_training)
svmPR = postResample(pred=svmPred, obs=fat_training) 
rmses_training = c(rmses_training,svmPR[1])
r2s_training = c(r2s_training,svmPR[2])
methods = c(methods,"SVM")

svmPred = predict(svmModel, newdata=absorp_testing)
svmPR = postResample(pred=svmPred, obs=fat_testing)
rmses_testing = c(rmses_testing,svmPR[1])
r2s_testing = c(r2s_testing,svmPR[2])

# Package the results up:
# 
res_training = data.frame( rmse=rmses_training, r2=r2s_training )
rownames(res_training) = methods

training_order = order( -res_training$rmse )

res_training = res_training[ training_order, ] # Order the dataframe so that the best results are at the bottom:
print( "Final Training Results" ) 
print( res_training )

res_testing = data.frame( rmse=rmses_testing, r2=r2s_testing )
rownames(res_testing) = methods

res_testing = res_testing[ training_order, ] # Order the dataframe so that the best results for the training set are at the bottom:
print( "Final Testing Results" ) 
print( res_testing )

# EPage 82 
resamp = resamples( list(knn=knnModel,svm=svmModel,mars=marsModel,nnet=nnetModel,avnnet=avNNetModel) )
print( summary(resamp) )

dotplot( resamp, metric="RMSE" )

print( summary(diff(resamp)) )
