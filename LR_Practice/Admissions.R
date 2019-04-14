library(ggplot2)
library(tidyverse)
library(textclean)
library(gridExtra)
library(GGally)
library(rlist)
library(dplyr)

rawdata <- read.csv("Admission_Predict.csv")
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)
rawdata <- rawdata %>% 
  dplyr::select(-serialno)


#Reserach Question: Create the best model to predict chance of admission


#=================Checking for missing data===================================

percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(rawdata, 1, percent_miss)
colmiss <- apply(rawdata, 2, percent_miss)

crit_part_miss <- subset(rawdata, rowmiss > 50)
crit_var_miss <- rawdata[colmiss > 50]
#not missing anything

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

#--------cat variables vs Chance of Admission 
multBoxPlots <- function(mydata, x_var, y_var) {
  bp <- ggplot(mydata, aes_(as.name(x_var), as.name(y_var))) +
      geom_boxplot()
    print(bp)
}

names(cat_vars)
x_vars <- names(cat_vars[,c(1,4,6,7)])
x_vars
mybplots <- list()
for(i in seq_along(x_vars)){
  mybplots[[i]] <- multBoxPlots(mydata = cat_vars, x_var = x_vars[i], y_var = "chanceofadmit")$plot
}

names(cat_vars)
ggpairs(cat_vars[c(1,4,5,6,7)])

# T tests (research)

t.test(chanceofadmit ~ research, data = cat_vars) # Very Sig

#ANOVA ( Uni Rating, lor_g5, sop_g5)

aov_cars <- cat_vars %>%
  dplyr::select(universityrating, lor_g5, sop_g5)

map(aov_cars, function(x) {
  summary(aov(chanceofadmit ~ x, data = cat_vars))
})

#all very significant 

#-----------Num variables vs chance of admission 

names(num_vars)

ggpairs(num_vars) # cgpa > gre > toefl , they're also all correlated with each other 

#======================REGRESSION ANALYSIS====================================

reg_data <- cbind(cat_vars, num_vars)
reg_data <- reg_data[,-c(2,3,5)]


r.square_list <- list()
#Regression with everything: 
full_model <- lm(chanceofadmit ~., data = reg_data)
summary(full_model)

theme_update(plot.title = element_text(hjust = 0.5)) 

ggplot(data = full_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = full_model, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals")+
  ggtitle("Full Model")

qqnorm(full_model$residuals)
qqline(full_model$residuals)



final_model <- lm(chanceofadmit ~ research + grescore + toeflscore + cgpa, data = reg_data )
summary(final_model)

ggplot(data = final_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = final_model, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals")+
  ggtitle("Final Model")

qqnorm(final_model$residuals)
qqline(final_model$residuals)

fm_residuals <- final_model$residuals
shapiro.test(fm_residuals) 

names(full_model)


r.square_list <- append(r.square_list, summary(full_model)$r.squared)
r.square_list <- append(r.square_list, summary(m1)$r.squared)

ggplot(data = r.square_df, aes(x = V1))+
  geom_bar(stat = "identity")


r.square_df <- as.data.frame(do.call(rbind, r.square_list))


reg_data <- reg_data %>%
  mutate(chanceofadmit_log = log10(chanceofadmit))%>%
  mutate(cgpa_log = log10(cgpa))

m1 <- lm(chanceofadmit_log ~ research + grescore + toeflscore + cgpa_log, data = reg_data )

ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals")+
  ggtitle("Final Model")

qqnorm(m1$residuals)
qqline(m1$residuals)


ggplot(data = reg_data, aes(x = cgpa, y = chanceofadmit))+
  geom_point()

       


#---log Trans

hist(reg_data$cgpa)
qqnorm(reg_data$cgpa)
qqline(reg_data$cgpa)

hist(reg_data$grescore)
qqnorm(reg_data$grescore)
qqline(reg_data$grescore)

hist(reg_data$toeflscore)
qqnorm(reg_data$toeflscore)
qqline(reg_data$toeflscore)

#let's log transform cgpa and see what happens 

reg_data <- reg_data %>% 
  mutate(chanceofadmit_cubed = chanceofadmit^3)%>%
  mutate(grescore2 = grescore^2)%>%
  mutate(toeflscore2 = toeflscore^2)%>%
  mutate(cgpa2 = cgpa^2)

View(head(reg_data))

final_model2 <- lm(chanceofadmit_cubed ~ research + grescore + grescore2 + toeflscore + toeflscore2 + cgpa + cgpa2, data = reg_data )
summary(final_model2)
summary(final_model)

ggplot(data = final_model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = final_model2, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals")+
  ggtitle("Final Model")

qqnorm(final_model$residuals)
qqline(final_model$residuals)

par(mfrow = c(1,1))

hist(reg_data$cgpa)
hist(reg_data$cgpa_log)


names(reg_data)


confint(full_model)
