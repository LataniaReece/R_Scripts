#Ch 18.2

library(caret)
data(oil)
data <- data.frame(fattyAcids, oilType = oilType)
#Predict oil type given the fatty acids 

#ANOVA
getfStats <- function(x,y)
{
  res.aov <- aov(y ~ x)
  sum_test = unlist(summary(res.aov))
  out <- c(fStat = sum_test["Pr(>F)1"], p = sum_test["F value1"])
  out
}

fVals <- apply(data[,-8], 2, getfStats,
               x = data$oilType)

# switch the dimensions 
fVals <- t(fVals)

#Relief 
library(CORElearn)
reliefValues <- attrEval(oilType ~., 
                         data = data,
                         estimator = 'ReliefFequalK',
                         ReliefIterations = 50)

# aov tests: linoleic > oleic > linolenic > stearic > palmitic
#relief: linoleic > oleic > stearic > palmitic > linolenic




