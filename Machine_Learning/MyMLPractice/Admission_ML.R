library(ggplot2); library(textclean); library(dplyr)
rawdata <- read.csv("Admission_Predict.csv")

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


#================Exploratory==============================

featurePlot(x = training[,-8],
            y = training$chanceofadmit,
            plot = "pairs")

#========Checking Principal Components==============
num_vars <- training %>%
  select(grescore, toeflscore, cgpa)
cat_vars <- training %>%
  select(universityrating, research, sop_g5, lor_g5)

names(training)

my_prc <- prcomp(num_vars, center = TRUE, scale = TRUE)
summary(my_prc)
my_prc$rotation
my_prc$sdev ^ 2

screeplot(my_prc, main = "Screeplot", xlab = "Components")
screeplot(my_prc, main = "Screeplot", type = "line")

biplot(my_prc)
abline(h=0, v=0)

my_var <- varimax(my_prc$rotation)

#=========Creating Data Frame with Principal Components===============
trg <- predict(my_prc, num_vars)
training_data <- data.frame(PC1 = trg[,1], cat_vars, chanceofadmit = training$chanceofadmit)
test <- predict(my_prc, testing)
testing_data <- data.frame(test, testing)

#=========More Exploratory===================
#Remember, low scores on PC1 = high scores on GRE, cgpa, toefl
qplot(PC1, chanceofadmit, data = training_data)
qplot(PC1, chanceofadmit, color = research, data = training_data)
qplot(PC1, chanceofadmit, color = universityrating, data = training_data)
qplot(PC1, chanceofadmit, color = lor_g5, data = training_data)
qplot(PC1, chanceofadmit, color = sop_g5, data = training_data)

par(mfrow=c(1,1))

boxplot(chanceofadmit ~ research, data = training_data)
qplot(chanceofadmit, color = research, data = training_data, geom = "density")
boxplot(chanceofadmit ~ universityrating, data = training_data)
qplot(chanceofadmit, color = universityrating, data = training_data, geom = "density")
boxplot(chanceofadmit ~ sop_g5, data = training_data)
qplot(chanceofadmit, color = sop_g5, data = training_data, geom = "density")
boxplot(chanceofadmit ~ lor_g5, data = training_data)
qplot(chanceofadmit, color = lor_g5, data = training_data, geom = "density")

#=========Finding Model===========================
library(caret)
full_model <- train(chanceofadmit^2 ~.,
                  method = "lm", data = training_data)
full_model
summary(full_model)
fm_model <- full_model$finalModel
AIC(fm_model)

plot(fm_model,2, pch = 19, cex = 0.5)
training["360",]

#checking covariate variability 
nsv <- nearZeroVar(training_data, saveMetrics = TRUE)

#Testing other models: 
#-sop
m1 <- train(chanceofadmit^2 ~PC1+universityrating+research,
            method = "lm", data = training_data)
m1
summary(m1)
m1_model <- m1$finalModel
AIC(m1_model)
plot(m1_model, 1, pch = 19, cex = 0.5)

final_model <- m1


#===============Using model on test data===============
pred <- predict(final_model, testing_data)
cor.test(testing_data$chanceofadmit, pred)
