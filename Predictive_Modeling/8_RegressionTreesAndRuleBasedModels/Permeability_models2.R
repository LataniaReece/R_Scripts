library(caret)
library(AppliedPredictiveModeling)

set.seed(0)

data(permeability)
data <- data.frame(fingerprints, permeability)
# Part (b):
# 
zero_cols = nearZeroVar( fingerprints )
print( sprintf("Found %d zero variance columns from %d",length(zero_cols), dim(fingerprints)[2]))
data = data[,-zero_cols] # drop these zero variance columns 

# Split this data into training and testing sets:
#
set.seed(0)
inTrain = createDataPartition(data$permeability, p=0.8, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

preProc_Arguments = c("center","scale")

# A K-NN model:
# 
set.seed(0)
knnModel = train(permeability ~., 
                 data = training,
                 method="knn", 
                 preProc=preProc_Arguments, 
                 tuneLength=10)

print('Finished knn Model')

nnGrid = expand.grid( .decay=c(0,0.01,0.1),
                      .size=1:10,
                      .bag=FALSE )
set.seed(0)
nnetModel = train(permeability ~.,
                  data = training,
                  method="nnet", 
                  preProc=preProc_Arguments,
                  linout=TRUE,
                  trace=FALSE,
                  MaxNWts=10 * (ncol(training[,-389])+1) + 10 + 1,
                  maxit=500)
saveRDS(nnetModel, 'permeability_nnet.rds')

print('Finished nnet Model')

marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
set.seed(0)
marsModel = train(permeability ~ .,
                  data = training,
                  method="earth",
                  preProc=preProc_Arguments,
                  tuneGrid=marsGrid)

saveRDS(marsModel, 'permeability_marsModel.rds')
print('Finished mars Model')

set.seed(0)
svmModel = train(permeability~., 
                 data = training,
                 method="svmRadial",
                 preProc=preProc_Arguments,
                 tuneLength=20)
saveRDS(svmModel, 'permeability_svm.rds')
print('Finished svm Model')






