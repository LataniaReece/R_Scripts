library(ggplot2)
library(tidyverse)
library(textclean)
library(GGally)
library(dplyr)

data <- read.csv("Admission_Predict.csv")
rawdata <- data
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)
rawdata <- rawdata %>% 
  dplyr::select(-serialno)

#Reserach Question: Create the best model to predict chance of admission

#===============Creating New dfs==============================

cat_vars <- rawdata %>%
  dplyr::select(universityrating, sop, lor, research,chanceofadmit)%>%
  mutate(sop_g5 = sop) %>%
  mutate(lor_g5 = lor)
cat_vars[,c(1:4, 6:7)] <- data.frame(apply(cat_vars[,c(1:4, 6:7)], 2, as.factor))
levels(cat_vars$sop_g5) <- c(1,1,2,2,3,3,4,4,5)
levels(cat_vars$lor_g5) <- c(1,1,2,2,3,3,4,4,5)

num_vars <- rawdata %>% 
  dplyr::select(grescore, toeflscore, cgpa, chanceofadmit)

#================Exploratory Analysis=============================

ggpairs(cat_vars)
ggpairs(num_vars) 

reg_data <- cbind(cat_vars[c(1,4,5,6,7)], num_vars[1:3])
str(reg_data)
train_data <- reg_data[1:200,]
test_data <- reg_data[201:400,]

#======================REGRESSION=================

full_model<- lm(chanceofadmit ~., data = train_data)
summary(full_model)

m1 <- lm(chanceofadmit ~ research + lor_g5 + grescore + toeflscore + cgpa,
         data =train_data)
summary(m1)

library(MASS)
bc <- boxcox(m1, lambda = seq(-5,5))
best.lam <- bc$x[which(bc$y==max(bc$y))]
detach("package:MASS", unload=TRUE)

m2 <- lm(chanceofadmit_squared ~ cgpa_squared + gre_squared, data =train_data)
summary(m2)

library(regclass)
VIF(m2)
detach("package:regclass", unload=TRUE)


train_data <- train_data %>%
  mutate(chanceofadmit_squared = chanceofadmit^2)%>%
  mutate(cgpa_squared = cgpa ^2)%>%
  mutate(gre_squared = grescore ^ 2)

ggplot(data = train_data, aes(x=grescore, y = chanceofadmit_squared))+
  geom_point()

library(stats)
test_data <- test_data %>%
  mutate(chanceofadmit_squared = chanceofadmit^2)%>%
  mutate(cgpa_squared = cgpa ^2)%>%
  mutate(gre_squared = grescore ^ 2)


pred_admission <- predict(m2, test_data, se.fit = TRUE)
test_data <- cbind(test_data, pred_admission$fit)
ggpairs(test_data[c(3, 12)]) #Good Model, 0.914 correlation between predicted and actual



