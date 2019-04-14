library(dplyr)
library(ggplot2)
library(statsr)
library(tidyverse)
library(gridExtra)

###Question: Predict low birth weight given the variables (mature, visits, marital, gender of baby, habit, whitemom)

rawdata <- nc
rq_data <- rawdata %>% 
  select(mature, visits, marital, gender, habit, whitemom, weight, lowbirthweight)

#==============================MISSING VALUES============================================================

#Figure out how many Na's , not enough to remove but showing how you would do it if there was. 

percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(rq_data, 1, percent_miss)
colmiss <- apply(rq_data, 2, percent_miss)

#row_crit_miss <- subset(rq_data, rowmiss > 15)
#col_crit_miss <-rawdata[rq_data > 16]

#==================================EXPLORATORY ANALYSIS================================================

#==================Relationship between Cat variables and Lowbirthweight: 
#Mature, Marital, Gender, habit, whitemom, visits

#For visits, I am going to make a new catgorical variable with 3 groups
rq_data <- rq_data %>%
  mutate(visits_g3 = as.factor(ifelse(visits < 10, "little visits", 
                                      ifelse(visits > 20, "many visits", "medium visits"))))
rq_data$visits_g3 <- factor(rq_data$visits_g3, levels = c("little visits", "medium visits", "many visits"))

#Chi Square Analysis
cat_vars <- rq_data %>%
  select(mature, marital, gender, habit, whitemom, visits_g3)
tables <- map(cat_vars, function(x) { 
  table(rq_data$lowbirthweight, x)})
map(tables, function(x) {chisq.test(x)})

#-----Visuals: 

theme_update(plot.title = element_text(hjust = 0.5)) 

mature_plot <- ggplot(data = rq_data, aes(x= mature, fill= lowbirthweight))+
  geom_bar(stat="count", position = "dodge")
habit_plot <- ggplot(data = subset(rq_data, !is.na(habit)), aes(x= habit, fill= lowbirthweight))+
  geom_bar(stat="count", position = "dodge")
marital_plot <- ggplot(data = subset(rq_data, !is.na(marital)), aes(x= marital, fill= lowbirthweight))+
  geom_bar(stat="count", position = "dodge")
wm_plot <- ggplot(data = subset(rq_data, !is.na(whitemom)), aes(x= whitemom, fill= lowbirthweight))+
  geom_bar(stat="count", position = "dodge")
gender_plot <- ggplot(data = rq_data, aes(x= gender, fill= lowbirthweight))+
  geom_bar(stat="count", position = "dodge")
visits_plot <- ggplot(data = subset(rq_data, !is.na(visits_g3)), aes(x= visits_g3, fill= lowbirthweight))+
  geom_bar(stat="count", position = "dodge")

grid.arrange(mature_plot, habit_plot, marital_plot, wm_plot, gender_plot, visits_plot, ncol = 2)

##================Relationship between Cat variables and Baby Weight: 
#Mature, Marital, Gender, habit, whitemom, visits

#-----Visuals 

ggplot(data = rq_data, aes(y=weight, x=mature))+
  geom_boxplot()
ggplot(data = subset(rq_data, !is.na(habit)), aes(y=weight, x=habit))+
  geom_boxplot()
ggplot(data = subset(rq_data, !is.na(marital)), aes(y=weight, x=marital))+
  geom_boxplot()
ggplot(data = subset(rq_data, !is.na(whitemom)), aes(y=weight, x=whitemom))+
  geom_boxplot()
ggplot(data = subset(rq_data, !is.na(gender)), aes(y=weight, x=gender))+
  geom_boxplot()
ggplot(data = subset(rq_data, !is.na(visits_g3)), aes(y=weight, x=visits_g3))+
  geom_boxplot()

###t tests:

#excluding visits since this has 3 levels
weight <- rq_data$weight
cat_vars %>% 
  select(-visits_g3) %>%
  lapply(function(x) {t.test(weight ~ x, var.equal= TRUE)})


##ANOVA

res.aov <- aov(weight ~ visits_g3, data = rq_data)
summary(res.aov)

#Sig: marital, whitemom, habit, gender, visits

