library(ISLR)
data("Hitters")
rawdata <- Hitters

rawdata$missing <- ifelse(is.na(rawdata$Salary), "NAs","No-NAs")
rawdata$missing <- as.factor(rawdata$missing)
check <- sample(rep(unique(rawdata$missing), each = 59))
table(check)

str(rawdata)

library(dplyr)
new_df <- rawdata %>% 
  group_by(missing) %>%
  sample_n(59)

table(new_df$missing)

table(rawdata$missing)

new_df$Salary <- NULL
logitMod <- glm(missing ~.,
                data = new_df,
                family=binomial)

summary(logitMod)

boxplot(new_df$PutOuts ~ new_df$missing)
boxplot(new_df$RBI ~ new_df$missing)
table(new_df$PutOuts, new_df$missing)

?Hitters


library(tidyverse)
boxplot(rawdata$AtBat ~ rawdata$missing)

library(caret)

pairs.panels(rawdata)

library(psych)
?pairs.panels
str(rawdata)

head(rawdata[,-c(19,21)])

complete_data <- complete.cases(rawdata)


sum(complete_data)
missing <- data[!complete_data,] #All have salary missing

?Hitters
data <- data[complete_data,]


#Data Partition
library(caret)
set.seed(250)
inTrain <- createDataPartition(y = data$Salary, 
                               p = 0.7,
                               list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain,]
