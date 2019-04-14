library(dplyr)
library(ggplot2)
library(GGally)


rawdata <- read.delim(url("https://newonlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/data/iqsize/index.txt"),
                      header = TRUE,
                      sep = "")
ggpairs(rawdata)

#Find the best model to predict PIQ

full_model <- lm(PIQ ~., data = rawdata)
summary(full_model)

m1 <- lm(PIQ ~ Brain + Height, data = rawdata)
summary(m1)

ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 5) +
  xlab("Residuals")

qqnorm(m1$residuals)
qqline(m1$residuals)
