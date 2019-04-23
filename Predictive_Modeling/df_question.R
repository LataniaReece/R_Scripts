#Transfer them to factors and then do this and see what happens 
Soybean data

levels(a ) <- levels(b)
colnames(a) <- levels(a)

library(mlbench)
data(Soybean)

library( caret ) 
zero_cols = nearZeroVar( Soybean )
colnames( Soybean )[ zero_cols ]
Soybean = Soybean[,-zero_cols] 

str(Soybean)
library(dplyr)
num_Soybean <- mutate_if(Soybean, is.factor, as.numeric)
predictors <- data.matrix(num_Soybean[,-1])
impute <- preProcess(predictors, 
                     method=c("knnImpute")) 
Sb_imputed <- predict(impute, predictors)
Sb_imputed<- data.frame(Sb_imputed)
str(Sb_imputed)

#two df = Soybean and Sb_imputed

#make a list with all the levels for the variables in Soybean 
var_levels <- c()
library(itertools)
for (i,j in enumerate(Soybean)){
  print(i)
}
  
check <- names(Soybean)[2]
Soybean$check
