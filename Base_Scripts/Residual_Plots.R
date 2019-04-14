#This is the best linear regression model, however, the r squared shows that there is not alot
#variablity explained. Let's look at the residual plots 

#Linearity

ggplot(data = m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")
#note that the lines in the data are just a function of the different groups.

#normality 

ggplot(data = m2, aes(x = .resid)) +
  geom_histogram(binwidth = 0.2) +
  xlab("Residuals")

qqnorm(m2$residuals)
qqline(m2$residuals)
