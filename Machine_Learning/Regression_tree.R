library(rpart)
library(rpart.plot)

#https://www.youtube.com/watch?v=MoBw5PiW56k
#In this example the person used on a few variables to make a decision tree
#target: sleep_total
#small sample so he didn't split into training and testing, used the same data twice 

df <- msleep[,c(3,6,10,11)]
m1 <- rpart(sleep_total ~., data = df, method = "anova")

#visualization
rpart.plot(m1, type = 3, digits = 3, fallen.leaves = TRUE)
p1 <- predict(m1, df) #again, no testing data here, just using original data (wouldn't do this in practice)

#Calculating mae - you would compare to other models to predict the best one
mae(df$sleep_total, p1)

#remember you wouldn't use accuracy in this situation because it is not a classification problem 

