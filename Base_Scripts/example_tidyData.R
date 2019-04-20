#https://youtu.be/1ELALQlO-yM?list=PL9HYL-VRX0oQOWAFoKHFQAsWAI3ImbNPk

load("C:\\Users\\reece\\Desktop\\R_Scripts\\Base_Scripts\\cases.rdata")
load("C:\\Users\\reece\\Desktop\\R_Scripts\\Base_Scripts\\storms.rdata")
load("C:\\Users\\reece\\Desktop\\R_Scripts\\Base_Scripts\\pollution.rdata")

#Storms = what the data should look like 
storms

library(tidyr)
#cases: 
cases
gather(cases, "year", "n", 2:4)

#pollution
pollution
spread(pollution, size, amount)

#showcasing separate and unite function
storm1 <- separate(storms, date, c('year', 'month', 'day'), sep = '-')
storm1
storm2 <- unite(storm1, 'date', year, month, day, sep = '-')
storm2

check <- data.frame(storm2)
check
str(check)
