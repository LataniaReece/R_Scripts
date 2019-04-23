#Ans 3.2

# Exercise 2 (EPage 54): 
#
library(mlbench)
data(Soybean)

library( caret ) 
zero_cols = nearZeroVar( Soybean )
colnames( Soybean )[ zero_cols ]
Soybean = Soybean[,-zero_cols] 

# Count how many NA's we have in each feature:
#
apply( Soybean, 2, function(x){ sum(is.na(x)) } )
#percentage 
apply( Soybean, 2, function(x) {(sum(is.na(x))/length(x))*100} )

# See if a class has more NA's than others:
#
Soybean$has_nans_in_sample = apply( Soybean[,-1], 1, function(x){ sum(is.na(x)) > 0 } )
#variable - 'has_nans_in_sample' is now variable # 34

table( Soybean[, c(1,34) ] )
names(Soybean)

str(Soybean)
# For imputation of data for the NA's ( I think MICE is better)
# 
library( caret )
#first remove the has_nan_in_sample column and convert factors to numeric 
Soybean$has_nans_in_sample <- NULL
library(dplyr)
predictors <- mutate_if(Soybean[,-1], is.factor, as.numeric)
impute <- preProcess(predictors, 
                     method=c("knnImpute")) 
Sb_imputed <- predict(impute, predictors)
Sb_imputed<- data.frame(Sb_imputed, Class = Soybean$Class)
str(Sb_imputed)


