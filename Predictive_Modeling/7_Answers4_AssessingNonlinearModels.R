save_plots = F

library(caret)
library(AppliedPredictiveModeling)

set.seed(0)

data(permeability)

# Part (b):
# 
zero_cols = nearZeroVar( fingerprints )
print( sprintf("Found %d zero variance columns from %d",length(zero_cols), dim(fingerprints)[2] ) )
fingerprints = fingerprints[,-zero_cols] # drop these zero variance columns 

# Split this data into training and testing sets:
#
training = createDataPartition( permeability, p=0.8 )

fingerprints_training = fingerprints[training$Resample1,]
permeability_training = permeability[training$Resample1]

fingerprints_testing = fingerprints[-training$Resample1,]
permeability_testing = permeability[-training$Resample1]

check <- data.frame(fingerprints_training)
# Build various nonlinear models and then compare performance:
# 
# Note we use the default "trainControl" bootstrap evaluations for each of the models below: 
#
preProc_Arguments = c("center","scale")

# A K-NN model:
# 
set.seed(0)
knnModel = train(x=fingerprints_training,
                 y=permeability_training, 
                 method="knn", 
                 preProc=preProc_Arguments, 
                 tuneLength=10)

# predict on training/testing sets
knnPred = predict(knnModel, newdata=fingerprints_training)
knnPR = postResample(pred=knnPred, obs=permeability_training)
rmses_training = c(knnPR[1])
r2s_training = c(knnPR[2])
methods = c("KNN")

knnPred = predict(knnModel, newdata=fingerprints_testing)
knnPR = postResample(pred=knnPred, obs=permeability_testing)
rmses_testing = c(knnPR[1])
r2s_testing = c(knnPR[2])


# A Neural Network model:
#
nnGrid = expand.grid( .decay=c(0,0.01,0.1),
                      .size=1:10,
                      .bag=FALSE )
set.seed(0)
nnetModel = train(x=fingerprints_training, 
                  y=permeability_training, 
                  method="nnet", 
                  preProc=preProc_Arguments,
                  linout=TRUE,
                  trace=FALSE,
                  MaxNWts=10 * (ncol(fingerprints_training)+1) + 10 + 1,
                  maxit=500)
saveRDS(nnetModel, 'nnet_perm_answer.rds')


nnetPred = predict(nnetModel, newdata=fingerprints_training)
nnetPR = postResample(pred=nnetPred, obs=permeability_training)
rmses_training = c(rmses_training,nnetPR[1])
r2s_training = c(r2s_training,nnetPR[2])
methods = c(methods,"NN")

nnetPred = predict(nnetModel, newdata=fingerprints_testing)
nnetPR = postResample(pred=nnetPred, obs=permeability_testing)
rmses_testing = c(rmses_testing,nnetPR[1])
r2s_testing = c(r2s_testing,nnetPR[2])


# Averaged Neural Network models:
#
set.seed(0)
avNNetModel = train(x=fingerprints_training,
                    y=permeability_training,
                    method="avNNet", 
                    preProc=preProc_Arguments,
                    linout=TRUE,
                    trace=FALSE,
                    MaxNWts=10 * (ncol(fingerprints_training)+1) + 10 + 1
                    , maxit=500)
?nnet
saveRDS(avNNetModel, 'avnnnet_perm_answer.rds')

avNNetPred = predict(avNNetModel, newdata=fingerprints_training)
avNNetPR = postResample(pred=avNNetPred, obs=permeability_training)
rmses_training = c(rmses_training,avNNetPR[1])
r2s_training = c(r2s_training,avNNetPR[2])
methods = c(methods,"AvgNN")

avNNetPred = predict(avNNetModel, newdata=fingerprints_testing)
avNNetPR = postResample(pred=avNNetPred, obs=permeability_testing)
rmses_testing = c(rmses_testing,avNNetPR[1])
r2s_testing = c(r2s_testing,avNNetPR[2])


# MARS model:
check = train(x=fingerprints_training,
                  y=permeability_training,
                  method="earth",
                  preProc=preProc_Arguments)
check
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
set.seed(0)
marsModel = train(x=fingerprints_training,
                  y=permeability_training,
                  method="earth",
                  preProc=preProc_Arguments,
                  tuneGrid=marsGrid)
?earth
saveRDS(marsModel, 'marsModel_perm_answer.rds')

marsPred = predict(marsModel, newdata=fingerprints_training)
marsPR = postResample(pred=marsPred, obs=permeability_training)
rmses_training = c(rmses_training,marsPR[1])
r2s_training = c(r2s_training,marsPR[2])
methods = c(methods,"MARS")

marsPred = predict(marsModel, newdata=fingerprints_testing)
marsPR = postResample(pred=marsPred, obs=permeability_testing)
rmses_testing = c(rmses_testing,marsPR[1])
r2s_testing = c(r2s_testing,marsPR[2])

# Lets see what variables are most important in the MARS model: 
varImp(marsModel)

# A Support Vector Machine (SVM):
#
set.seed(0)
svmModel = train(x=fingerprints_training,
                 y=permeability_training,
                 method="svmRadial",
                 preProc=preProc_Arguments,
                 tuneLength=20)

saveRDS(svmModel, 'svmModel_perm_answer.rds')

svmPred = predict(svmModel, newdata=fingerprints_training)
svmPR = postResample(pred=svmPred, obs=permeability_training) 
rmses_training = c(rmses_training,svmPR[1])
r2s_training = c(r2s_training,svmPR[2])
methods = c(methods,"SVM")

svmPred = predict(svmModel, newdata=fingerprints_testing)
svmPR = postResample(pred=svmPred, obs=permeability_testing)
rmses_testing = c(rmses_testing,svmPR[1])
r2s_testing = c(r2s_testing,svmPR[2])

# Package the results up:
# 
res_training = data.frame( rmse=rmses_training, r2=r2s_training )
rownames(res_training) = methods

training_order = order( -res_training$rmse )

res_training = res_training[ training_order, ]
# Order the dataframe so that the best results are at the bottom:
print( "Final Training Results" ) 
print( res_training )

res_testing = data.frame( rmse=rmses_testing, r2=r2s_testing )
rownames(res_testing) = methods

res_testing = res_testing[ training_order, ] 
# Order the dataframe so that the best results for the training set are at the bottom:
print( "Final Testing Results" ) 
print( res_testing )

# EPage 82 
resamp = resamples( list(svm=svmModel,knn=knnModel,nnet=nnetModel,avnnet=avNNetModel,mars=marsModel) )
print( summary(resamp) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/chap_7_prob_4_resamp_dotplot.eps", onefile=FALSE, horizontal=FALSE) }
dotplot( resamp, metric="RMSE" )
if( save_plots ){ dev.off() }

print( summary(diff(resamp)) )
