library(caret)
data(BloodBrain)

#Understanding data---------------
data <- bbbDescr
str(data)

#selecting numeric data 
ints <- unlist(lapply(data, is.integer))
nums <- data[, !ints]
ints <- data[,ints]

#checking if they are cat variables 
freq_dist <- apply(ints, 2, function(x) table(x))
freq_dist

#degenerate distributions-------------
#numeric
library(e1071)
skew_values <- apply(nums, 2, skewness)
skew_values <- data.frame(names = names(skew_values), skew_value = skew_values, row.names = NULL)
head(skew_values)

#checking for skewed 
sort_skew <- skew_values[order(-abs(skew_values$skew_value)),]
head(sort_skew, n = 25)


library(caret)

#negative values - wnsa2
temp <- BoxCoxTrans(data$wnsa2)
temp #Lambda could not be estimated; no transformation is applied
summary(data$wnsa2)

temp<- BoxCoxTrans(data$wnsa2 + 3200) #used the most negative and added to all of them 
temp

wnsa2Trans <- predict(temp, data$wnsa2+3200)
hist(wnsa2Trans)
skewness(data$wnsa2)
skewness(wnsa2Trans)

#factor like variable w/ 0 values- vsa_acid (most skewed)
temp <- BoxCoxTrans(data$vsa_acid)
temp #Lambda could not be estimated; no transformation is applied
summary(data$vsa_acid)

temp<- BoxCoxTrans(data$vsa_acid + 1) #added 1 to all values
temp

vsa_acidTrans <- predict(temp, data$vsa_acid + 1)
hist(vsa_acidTrans)
skewness(data$vsa_acid)
skewness(vsa_acidTrans) #didn't really do anything 

#factor like variable w/ 0 values- slogp_vsa6 (less skewed)
temp <- BoxCoxTrans(data$slogp_vsa6)
temp #Lambda could not be estimated; no transformation is applied
summary(data$slogp_vsa6)

temp<- BoxCoxTrans(data$slogp_vsa6 + 1) #added 1 to all values
temp

slogp_vsa6Trans <- predict(temp, data$slogp_vsa6 + 1)
hist(slogp_vsa6Trans)
skewness(data$slogp_vsa6)
skewness(slogp_vsa6Trans) #did better than the one above 

#regular with 0 values - peoe_vsa.2.1 (most skewed)
temp <- BoxCoxTrans(data$peoe_vsa.2.1)
temp #Lambda could not be estimated; no transformation is applied
summary(data$peoe_vsa.2.1)

temp<- BoxCoxTrans(data$peoe_vsa.2.1 + 1) #added 1 to all values
temp

peoe_vsa.2.1Trans <- predict(temp, data$peoe_vsa.2.1 + 1)
hist(peoe_vsa.2.1Trans)
skewness(data$peoe_vsa.2.1)
skewness(peoe_vsa.2.1Trans) #didn't do much 

#correlations 
cors <- cor(nums)
library(corrplot)
corrplot(cors, type = 'upper')

?corrplot

#categoric 
#changing to factor variables
library(dplyr)
ints <- ints %>% mutate_if(is.integer,as.factor)
str(ints)

#Frequency Distribution
freq_dist <- apply(ints, 2, function(x) table(x))
freq_dist

#Freq Bar chart - my func
library(ggplot2)
multbarcharts <- function(mydata, x_var) {
  bp <- ggplot(mydata, aes_(as.name(x_var))) +
    geom_bar(stat = 'count')
  print(bp)
}
x_vars <- names(ints)
x_vars

mybarcharts <- list()
for(i in seq_along(x_vars)){
  mybarcharts[[i]] <- multbarcharts(mydata = ints, x_var = x_vars[i])$plot
}

