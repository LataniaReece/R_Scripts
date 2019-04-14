library(mice)
library(VIM)

#Data---------------------------
data <- read.csv("vehicleMiss.csv", header = T)
str(data)
summary(data)

#Missing Data---------------------
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(data, 2, p)
md.pattern(data)
md.pairs(data)
marginplot(data[,c('Mileage', 'lc')])
summary(data)
par(mar = c(4,4,4,4))

#Impute----------------------------
impute <- mice(data[,2:7], m = 3, seed = 123)
impute
impute$imp$Mileage

#Complete Data----------------------
newdata <- complete(impute, 1)

#Distribution of observed/imputed values----
stripplot(impute, pch= 20, cex = 1.2)
xyplot(impute, lc ~ lh | .imp, pch = 20, cex = 1.4)

