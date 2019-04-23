library(mlbench)
data(Glass)
str(Glass)

grep('Type', names(Glass))

#correlation----
glass_cor <- cor(Glass[,-10])


#Visuals-----
#corrplot
library(corrplot)
corrplot(glass_cor)
?corrplot
#Scatterplot matrix
library(GGally)
ggpairs(Glass[,-10])


#skewness-------
library(e1071)
skew_values <- apply(Glass[,-10], 2, skewness)
skew_values

hist(log(Glass$K))
check <- log(Glass$K+1)
skewness(check)
hist(Glass$K)
hist(Glass$Ba)
hist(Glass$Ca)

#Boxcox transformations------
library(caret)
#K
temp <- BoxCoxTrans(Glass$K + 1)
Glass$KTrans <- predict(temp, Glass$K+1)
hist(Glass$KTrans)
skewness(Glass$K)
skewness(Glass$KTrans)

#Ba - didn't really help as much
temp <- BoxCoxTrans(Glass$Ba + 1)
Glass$BaTrans <- predict(temp, Glass$Ba+1)
hist(Glass$BaTrans)
skewness(Glass$Ba)
skewness(Glass$BaTrans)

#Ca
temp <- BoxCoxTrans(Glass$Ca)
Glass$CaTrans <- predict(temp, Glass$Ca)
hist(Glass$CaTrans)
skewness(Glass$Ca)
skewness(Glass$CaTrans)

#Fe
temp <- BoxCoxTrans(Glass$Fe+1)
Glass$FeTrans <- predict(temp, Glass$Fe+1)
hist(Glass$Fe)
hist(Glass$FeTrans)
skewness(Glass$Fe)
skewness(Glass$FeTrans)



