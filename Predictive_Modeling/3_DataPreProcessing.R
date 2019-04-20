library(AppliedPredictiveModeling)
data("segmentationOriginal")

#They made a variable for training and testing
segData <- subset(segmentationOriginal, Case == 'Train')

#removing variables class and cell id from main object 
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
#removing those columns 
segData <- segData[,-(1:3)]

#Also removing binary versions of the predictors which contained the word 
#"status" in it 
statusColNum <- grep('Status', names(segData))
segData <- segData[,-statusColNum]

#TRANSFORMATIONS---------

#Assessing skewness 
library(e1071)
#for one predictor: 
skewness(segData$AngleCh1)
#for all predictors use the apply function - use as a guide to 
#visualize distributions
skewValues <- apply(segData, 2, skewness)
head(skewValues)

#BoxCox 
library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

#Original Data 
head(segData$AreaCh1)
#After Transformation 
predict(Ch1AreaTrans, head(segData$AreaCh1))

#using it in the preprocess function 
trans <- preProcess(segData, method = c('BoxCox', 'center', 'scale', 'pca'))
trans

#Applied the transformation: 
transformed <- predict(trans, segData)
head(transformed[,1:5])

#FILTERING-------------------------
#near zero variance
nearZeroVar(segData)
#if filters should be removed it would appear here 

#between-predictor correlations
correlations <- cor(segData)
dim(correlations)
correlations[1:4, 1:4]

#visualize the correlation structure 
library(corrplot)
corrplot(correlations, order = 'hclust')

#CREATING DUMMY VARIABLES 
data(cars)
cars$type <- names(cars[14:18])[max.col(cars[14:18])]
cars$type <- as.factor(cars$type)

simpleMod <- dummyVars(~Mileage + type, 
                       data = cars, 
                       levelsOnly = TRUE)
simpleMod
predict(simpleMod, head(cars))
