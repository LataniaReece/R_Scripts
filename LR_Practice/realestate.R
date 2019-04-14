library(plyr)
library(dplyr)
library(ggplot2)
library(GGally)
library(rlist)
library(purrr)
library(broom)

#RQ: Find the best model for predicting sales 

rawdata <- read.delim(url("https://newonlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/data/realestate/index.txt"),
                      header = TRUE, sep = "")
names(rawdata) <- tolower(names(rawdata))
rawdata[c(3:7, 9:10, 12)] <- data.frame(apply(rawdata[c(3:7, 9:10, 12)], 2, factor))
rawdata$style <- factor(rawdata$style, levels =c(1:11))

#----Regrouping catgeorical Variables (Style, Beds, Baths and Garage)
rawdata <- rawdata %>%
  mutate(style_g4 = style)
levels(rawdata$style_g4) <- c(1,1,2,2,3,3,4,4,4,4,4)
rawdata$style_g4 <- revalue(rawdata$style_g4, c("4"="4+"))

rawdata <- rawdata %>%
  mutate(baths_g5= baths)
levels(rawdata$baths_g5) <- c(1,2,3,4,5,5,5)
rawdata$baths_g5 <- revalue(rawdata$baths_g5, c("5"="5+"))

rawdata <- rawdata %>%
  mutate(beds_g6 = beds)
levels(rawdata$beds_g6) <- c(1,2,3,4,5,6,6)
rawdata$beds_g6 <- revalue(rawdata$beds_g6, c("6"="6+"))

rawdata <- rawdata %>%
  mutate(garage_g4 = garage)
levels(rawdata$garage_g4) <- c(0,1,2,3,3,3,3)
rawdata$garage_g4 <- revalue(rawdata$garage_g4, c("3"="3+"))


#--------------Regression ---------

reg_data <- rawdata %>%
  select(year, sqfeet, lot, highway, air, pool, quality, style_g4, beds_g6, baths_g5, garage_g4)
str(reg_data)

#Full model 
full_model <- lm(sqfeet ~., data = reg_data)
summary(full_model)

ggplot(data = full_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = full_model, aes(x = .resid)) +
  geom_histogram(binwidth = 0.2) +
  xlab("Residuals")

qqnorm(full_model$residuals)
qqline(full_model$residuals)

fm_residuals <- full_model$residuals
shapiro.test(fm_residuals)

# - highway
m1 <- lm(sqfeet ~ year + lot + air + pool + quality + style_g4 + beds_g6 + baths_g5 + garage_g4, data = reg_data)
summary(m1)

#-garage
m2 <- lm(sqfeet ~ year + lot + air + pool + quality + style_g4 + beds_g6 + baths_g5, data = reg_data)
summary(m2)

#-year
m3 <- lm(sqfeet ~ lot + air + pool + quality + style_g4 + beds_g6 + baths_g5, data = reg_data)
summary(m3)

#-pool
m4 <- lm(sqfeet ~ lot + air + quality + style_g4 + beds_g6 + baths_g5, data = reg_data)
summary(m4)

#-air 
m5 <- lm(sqfeet ~ lot + quality + style_g4 + beds_g6 + baths_g5, data = reg_data)
summary(m5)

# m5
ggplot(data = m5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m5, aes(x = .resid)) +
  geom_histogram(binwidth = 0.2) +
  xlab("Residuals")

qqnorm(m5$residuals)
qqline(m5$residuals)

m5_residuals <- m5$residuals
shapiro.test(m5_residuals)

#Checking if year is needed
m5_nums <- data.frame(year = reg_data$year, residuals = m5$residuals)

ggplot(data = m5_nums, aes(x = year, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Year") +
  ylab("Residuals")

#Looking at rsquare, adj r square, p value and f stats
models <- list(full_model, m1, m2, m3, m4, m5, m6)

rsquare_values <- map(models, function(x) {
 rsquares <- cbind(base::summary(x)$r.squared)
})

adj_rsquare_values <- map(models, function(x) {
  adj.rsquares <- cbind(base::summary(x)$adj.r.squared)
})

p_values <- map(models, function(x) {
  pvalue <- cbind(broom::glance(x)$p.value)
})

f_stat <- map(models, function(x) {
  fstat <- cbind(broom::glance(x)$statistic)
})

models_sum <- data.frame(Model = c("Full Model", "M1", "M2", "M3", "M4", "M5", "M6"), 
                            R.squares = matrix(unlist(rsquare_values)),
                            Adj.r.squares =matrix(unlist(adj_rsquare_values,)),
                         P.values = matrix(unlist(p_values)),
                         F.stat = matrix(unlist(f_stat)))

map(models_sum[,2:5], function(x) {
  ggplot(data = models_sum, aes(x = Model, y = x))+
    geom_bar(stat = "identity")
})


#----Transforming the data: 
summary(m5)
plot(m5)

bc <- boxcox(m5)
best.lam <- bc$x[which(bc$y==max(bc$y))]

m5_log <- lm(log10(sqfeet) ~ lot + quality + style_g4 + beds_g6 + baths_g5, data = reg_data)
summary(m5_log)

plot(m5_log)

ggplot(data = m5_log, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m5_log, aes(x = .resid)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Residuals")

qqnorm(m5_log$residuals)
qqline(m5_log$residuals)

m5log_residuals <- m5_log$residuals
shapiro.test(m5log_residuals)

final_model <- lm(sqfeet_log ~ quality + style_g4 + beds_g6 + baths_g5, data = reg_data)
summary(final_model)

plot(final_model)


fmodel_residuals <- final_model$residuals
shapiro.test(fmodel_residuals)



