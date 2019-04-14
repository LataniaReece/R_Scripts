#Want the best model for predicting chance of admissions

#Data-----------------
rawdata <- read.csv("Admission_Predict.csv")
library(textclean)
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)

#Fixing Factor Variables
rawdata <- rawdata %>% 
  mutate(sop_g5 = sop, lor_g5 = lor) %>%
  select(-c(serialno, sop, lor))
rawdata[,c(3,5,7:8)] <- lapply(rawdata[,c(3,5,7:8)], factor)
levels(rawdata$sop_g5) <- rep(1:5, each = 2)
levels(rawdata$lor_g5) <- rep(1:5, each = 2)

str(rawdata)

#Exploratory---------------
library(caret)
featurePlot(x = rawdata[,-6],
            y=rawdata$chanceofadmit,
            plot = "pairs")

hist(rawdata$chanceofadmit) 
#seems that >0.5 is higher in this case gonna create a new variable
rawdata$highchance <- as.factor(ifelse(rawdata$chanceofadmit >0.5, 1, 0))
table(rawdata$highchance) 

boxplot(rawdata$chanceofadmit)
abline(h=mean(rawdata$chanceofadmit), col = "red")

#Visualizing Outliers
outlier_values <- boxplot.stats(rawdata$chanceofadmit)$out  # outlier values.
outlier_values
boxplot(rawdata$chanceofadmit, boxwex=0.6)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

temp <- rawdata %>%
  arrange(chanceofadmit)
#I don't feel like this score is that much of an outlier

#Trying Models------------------
rawdata$highchance <- NULL
data <- rawdata

#Data Partition 
library(caret)
set.seed(5)
inTrain <- createDataPartition(data$chanceofadmit, p = 0.7, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#linear regression 
ctrl = trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 3)

#ONE
set.seed(5)
lm_fit <- train(chanceofadmit~.,
                data = training,
                method = "lm",
                trControl = ctrl)
lm_fit
summary(lm_fit)

#TWO
set.seed(5)
lm_fit2 <- train(chanceofadmit~.-universityrating-sop_g5,
                data = training,
                method = "lm",
                trControl = ctrl)
lm_fit2
summary(lm_fit2)

#checking variance inflation 
library(regclass)
VIF(lm_fit$finalModel)


#Decision Trees - caret didn't work
# set.seed(5)
# tree_fit <- train(chanceofadmit ~.,
#                   data = training, 
#                   method = "rpart",
#                   trControl = ctrl,
#                   tuneGrid = expand.grid(cp = seq(0.1,1,0.1)))
# 
# plot(tree_fit)
# tree_fit
# summary(tree_fit)
# 
# library(rpart.plot)
# rpart.plot(tree_fit$finalModel)

#using r part 
library(rpart)
set.seed(5)
tree_fit<- rpart(chanceofadmit~., data = training, method = "anova")
rpart.plot(tree_fit)
yhat <- predict(tree_fit, newdata = testing)
postResample(yhat, testing$chanceofadmit) #gives RMSE, Rsquared, MAE 


