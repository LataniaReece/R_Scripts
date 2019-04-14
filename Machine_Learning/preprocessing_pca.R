library(caret); library(kernlab); data (spam)
inTrain <- createDataPartition(y = spam$type, 
                               p = 0.75, list = FALSE)
training <- spam[inTrain,]; testing <- spam[-inTrain,]

M <- abs(cor(training[,-58])) #correlation matrix of the variables 
diag(M) <- 0 #sets diagonals of correlation matrix to 0, since diagnoals = correlation with self
which(M > 0.8, arr.ind = T)


plot(spam[,34], spam[,32]) #Just choosing these two, they are highly correlated
plot(spam[,40], spam[,32]) 
plot(spam[,40], spam[,34])

#Small exmaple of PCA 
smallSpam <- spam[,c(32,34,40)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

prComp$rotation #The first principal component involves adding all three variables together
#PC1 = 0.57num857 + 0.57num415 + 0.58direct 

#Method 1 : PCA on entire data (not reponse variable)
typeColor <- ((spam$type =="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1)) #he did log because some of the variales were very skewed
plot(prComp$x[,1], prComp$x[,2], col = typeColor, xlab = "PC1", ylab = "PC2")

#Method 2: PCA on entire data 
preProc <- preProcess(log10(spam[,-58]+1), method = "pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#Preprocessing with PCA 
preProc <- preProcess(log10(training[,-58]+1), method = "pca", pcaComp= 2)
trainPC <- predict(preProc, log10(training[,-58]+1)) #these are my principal components that I trained to the dataset 
#using the model of creating principal components that I defined above 
modelFit <- train(training$type ~., method = "glm", data = trainPC) #now fit a model to the principal components 

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))
