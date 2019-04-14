
rawdata <- read.csv("student-mat.csv", header = TRUE, sep = ";")
#RQ: Best model to predict final grade (there's no missing data)

#==================Fixing variables
library(dplyr)
library(tidyverse)

num_vars <- rawdata %>%
  select(absences, G1, G2, G3, age)
rawdata <- as.data.frame(map(rawdata %>%
                               select(-c(names(num_vars))), as.factor))
rawdata <- cbind(rawdata, num_vars)

reg_data <- rawdata %>% 
  select(-c(G1, G2))

#==================Splitting Data
set.seed(1818)
n = nrow(reg_data)
trainIndex = sample(1:n, size = (n/2), replace=FALSE)
train = reg_data[trainIndex ,]
test = reg_data[-trainIndex ,]
#================REGRESSION=======================

full_model <- lm(G3 ~., data = train)


m1 <- lm(G3 ~.-Walc-address-reason-goout-famrel-guardian-traveltime-Mjob-nursery-Fjob-Fedu-studytime
         -Dalc-paid-Pstatus-higher-internet-activities-absences-school,
         data = train)
summary(m1)
head(sort((summary(m1)$coefficients[,4]), decreasing = TRUE), n = 10)

par(mfrow = c(1,2))

boxplot(reg_data$G3 ~ reg_data$school)
)

chi_results <- chisq.test(reg_data$G3,reg_data$school)
chi_results

cor.test(reg_data$G3, reg_data$age)
plot(reg_data$age, reg_data$G3)


table(reg_data$Medu)

library(MASS)
m2 <- lm((G3+1) ~.-freetime-Fjob-Dalc-Walc-guardian-reason-famrel-goout-internet-traveltime-health-schoolsup-nursery-Pstatus
         , data = train)
bc <- boxcox(m2)
bc.lam <- bc$x[which(bc$y==max(bc$y))]

pred_vars <- train %>%
  dplyr::select(school, sex, address, famsize, Medu, Fedu, Mjob,
                studytime, failures, famsup, paid, activities, higher, romantic, absences, age, G3)

library(GGally)
ggpairs(pred_vars)

#Removed famsup, paid, activites, famsize, address, sex and school based on the pics
m1 <- lm(G3 ~ Mjob + studytime + failures + romantic, data = train)
summary(m1)

confint(m1)





