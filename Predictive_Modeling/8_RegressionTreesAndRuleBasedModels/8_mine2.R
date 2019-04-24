#Use a simulation to show tree bias with different granularities 
library(ggplot2); library(textclean); library(dplyr)
rawdata <- read.csv("C:\\Users\\reece\\Desktop\\R_Scripts\\R_Scripts\\Machine_Learning\\Admission_Predict.csv")

names(rawdata) <- strip(names(rawdata), lower.case = TRUE)
rawdata <- rawdata %>% 
  select(-serialno)

#Reserach Question: Create the best model to predict chance of admission

#================Fixing Variables and selecting the ones I want==================
rawdata <- rawdata %>%
  mutate(sop_g5 = sop) %>%
  mutate(lor_g5 = lor)
rawdata[,c(3,4,5,7,9,10)] <- data.frame(apply(rawdata[,c(3,4,5,7,9,10)] , 2, as.factor))
levels(rawdata$sop_g5) <- c(1,1,2,2,3,3,4,4,5)
levels(rawdata$lor_g5) <- c(1,1,2,2,3,3,4,4,5)

data <- rawdata %>%
  select(grescore, toeflscore, universityrating, cgpa, research, sop_g5, lor_g5, chanceofadmit)

#=================Creating Training and testing set======================
set.seed(1818)
trainIndex <- createDataPartition(y=data$chanceofadmit,
                                  p = 0.7, list = FALSE)
training <- data[trainIndex,]
testing <- data[-trainIndex,]

#Trees----------------------- 
str(training)

#RandomForests
model1 = randomForest( chanceofadmit ~ ., data=training, importance=TRUE, ntree=1000 )
rfImp1 = varImp(model1, scale=FALSE)
rfImp1 = rfImp1[ order(-rfImp1), , drop=FALSE ]
print(rfImp1)

#ConditionalForests
library(party)

model1 = cforest(chanceofadmit ~ ., data=training)
cfImp1 = varImp(model1)
cfImp1 = cfImp1[ order(-cfImp1), , drop=FALSE ] 
print(cfImp1)


#Boosted Trees
model1 = gbm( chanceofadmit ~ ., data=training, distribution="gaussian", n.trees=1000 ) 
print(summary(model1,plotit=F)) # the summary method gives variable importance ... 

#removing gre and toeflscore to see how that effects cgpa's varimp
new_training <- training %>%
  select(-c(grescore, toeflscore))

#RandomForests
model1 = randomForest( chanceofadmit ~ ., data=new_training, importance=TRUE, ntree=1000 )
rfImp1 = varImp(model1, scale=FALSE)
rfImp1 = rfImp1[ order(-rfImp1), , drop=FALSE ]
print(rfImp1)
