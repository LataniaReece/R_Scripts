library(dplyr)
library(ggplot2)
library(tidyverse)
library(textclean)
library(gridExtra)
library(GGally)

#https://newonlinecourses.science.psu.edu/stat501/node/299/
# Predict Height with the other variables (LeftArm + LeftFoot + HeadCirc + nose)

rawdata <- read.table(url("https://newonlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/data/Physical/index.txt"),
                      header = TRUE, sep = "")

#Checking the VIF and Confidence intervals for the model 
full_model <- lm(Height~ LeftArm + LeftFoot + HeadCirc + nose, data = rawdata)
summary(full_model)
VIF(full_model)
confint(full_model)

#Chekcing the entire ANOVA table: 
aov_lm <- aov(full_model)
summary(aov_lm)


