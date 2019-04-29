#Ch 18 computing 

library(AppliedPredictiveModeling)
data(solubility)
#Binary predictors have "FP" in front of them, binary fingerprint predictors 

#Numerical Outcomes-----------------------------------------------

#Numerical Predictors (Numerical Outcomes):
fpCols <- grepl('FP', names(solTrainXtrans))
numericPreds <- names(solTrainXtrans)[!fpCols]

#Apply correlation function to numerical predictors (Pearson)
corrValues <- apply(solTrainXtrans[,numericPreds], 2,
                    function(x,y) cor (x, y), y = solTrainY)
head(corrValues)

#Apply correlation function to numerical predictors (Spearman)
corrValues <- apply(solTrainXtrans[,numericPreds], 2,
                    function(x,y) cor (x, y, method = 'spearman'), y = solTrainY)
head(corrValues)

#LOESS Smoother 
smoother <- loess(solTrainY ~ solTrainXtrans$NumCarbon)
smoother

#use lattice function to display loess fit 
library(lattice)
xyplot(solTrainY ~ solTrainXtrans$NumCarbon,
       type = c('p', 'smooth'),
       xlab = '# Carbons',
       ylab = 'Solubility')

#Caret FitlerVarImp Function, nonpara = TRUE -> nonparametric regression 
#-> creates a LOESS model for each predictor and quantifies the relationship with the outcome 
library(caret)
loessResults <- filterVarImp(x = solTrainXtrans[, numericPreds],
                             y = solTrainY,
                             nonpara = TRUE)
head(loessResults)

#MIC - mine function computes several quantities including the MIC value
library(minerva)
micValues <- mine(solTrainXtrans[,numericPreds], solTrainY)
micValues <- micValues$MIC
rownames(micValues) <- names(solTrainXtrans[,numericPreds])

#Categorical Predictors (Numerical Outcomes):
getTstats <- function(x,y)
  {
    tTest <- t.test(y ~ x)
    out <- c(tStat = tTest$statistic, p = tTest$p.value)
    out
  }
tVals <- apply(solTrainXtrans[, fpCols], 2, getTstats,
               y = solTrainY)
# switch the dimensions
tVals <- t(tVals)
head(tVals)

#Categorical Outcomes-----------------------------------------------
#Numerical Predictors (Categorical Outcomes):
library(caret)
data("segmentationData")

#Extraction the training data 
cellData <- subset(segmentationData, Case == 'Train')
cellData$Case <- cellData$Cell <- NULL

#use filterVarImp to calculate the area under the ROC curve
rocValues <- filterVarImp(x = cellData[, -1],
                          y = cellData$Class)
head(rocValues)

#Column is created for each class
library(pROC)
roc_list <- apply(cellData[, -1], 2, function(x,y) roc(x, y), x = cellData$Class)
auc_list <- lapply(roc_list, auc)

library(CORElearn)
#Relief 
reliefValues <- attrEval(Class ~., data = cellData,
                         #There are many relief methods available. see attrEval
                         estimator = 'ReliefFequalK',
                         ## The number of instance: 
                         ReliefIterations = 50)
head(reliefValues)
#Relief - Permutation Approach
perm <- permuteRelief(x = cellData[, -1],
                      y = cellData$Class,
                      nperm = 500, 
                      estimator = 'ReliefFequalK',
                      ReliefIterations = 50)

permRelief <- perm$permutations

permRelief$Var1 <- names(cellData[,-1])
  
check <- permuteRelief(x = cellData[,2:3],
                      y = cellData$Class,
                      nperm = 500, 
                      estimator = 'ReliefFequalK',
                      ReliefIterations = 50)
hey <- check$permutations
tail(hey)

#Information Gain
InfGain <- attrEval(Class ~., data = cellData,
                    estimator = 'InfGain')

#Histogram 
histogram(~ value|Predictor, 
          data = perm$permutations)
#Standardized version of the scores, rep the number of standard deviations that the observed 
#ReliefF values are from the center of the permuted distribution : 
head(perm$standardized)

#MIC stat - must have a binary encoding of the class
micValues <- mine(x = cellData[,-1],
                  y = ifelse(cellData$Class == 'PS', 1, 0))
micValues <- (micValues$MIC)
rownames(micValues) <- names(cellData[,-1])

#Odds Ratio 
#using binary data 
data <- read.csv("C:\\Users\\reece\\Desktop\\R_Scripts\\Machine_Learning\\binary.csv")
data$admit <- ifelse(data$admit == 0, 'Not Admit', 'Admit')

#Creating a two class predictor for gre for demo purposes 
data$greg2 <- ifelse(data$gre < 600, 'Low', 'High')

#2 classes odds ratio and fisher test 
greAdmitTable <- table(data$greg2, data$admit)
fisher.test(greAdmitTable)

#Fisher.test (more than 2 classes) - does not give odds ratio
admitRankTable <- table(data$rank, data$admit)
fisher.test(admitRankTable)

#Can also do a  chi square test 
chisq.test(greAdmitTable)
chisq.test(admitRankTable)


#GET ANOVA STATS
getfStats <- function(x,y)
{
  aovTest <- aov(y ~ x)
  sum_test = unlist(summary(res.aov))
  out <- c(fStat = sum_test["Pr(>F)1"], p = sum_test["F value1"])
  out
}

fVals <- apply(data[,-8], 2, getfStats,
               x = data$oilType)

# switch the dimensions 
fVals <- t(fVals)
head(fVals)




 