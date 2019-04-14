#URL: https://youtu.be/oVwUpuALP78

library(dplyr)
library(hflights)
data(hflights)
data <- hflights

#select----------------------
f1 <- select(data, FlightNum, ArrTime, DepTime)
f1 <- select(data, 5,6,8)
f1 <- select(data, contains("Time"))
f1 <- select(data, Year:ArrTime)
f1 <- select(data, 1:6)
f1 <- select(data, starts_with("Day"), ends_with("Time"))

#mutate-----------------------
f1 <- mutate(data, ActualGroundTime = ActualElapsedTime - AirTime)
f1 <- mutate(f1 , Averagespeed = Distance / AirTime * 60)
f1 <- mutate(f1, TotalTaxii = TaxiIn + TaxiOut)
f1 <- mutate(f1, TimeLoss = ArrDelay + DepDelay)

#filter-------------------------
f1 <- filter(data, Distance > 3000)
f1 <- filter(data, UniqueCarrier %in% c("oo", "US", "AA"))
f1 <- filter(data, TaxiIn + TaxiOut > AirTime)
f1 <- filter(data, DepTime <500|ArrTime >2220)
f1 <- filter(data, Dest =="JFK" & Cancelled ==1)

#Arrange-------------------------
f1 <- arrange(data, DepDelay)
f1 <- arrange(data, AirTime)
f1 <- arrange(data, DepDelay + ArrDelay)

#summarize-----------------------
summarize(data, min_dist = min(Distance), max_dist = max(Distance))
summarize(data, earliest= min(ArrDelay, na.rm = T), average = mean(ArrDelay, na.rm = T),
          latest = max(ArrDelay, na.rm = T), sd = sd(ArrDelay, na.rm = T))

#Pipe-----------------------------
f1 <- data %>% select(contains("Time")) %>% filter(AirTime > 60)
f1 <- data %>% filter(UniqueCarrier == "WN") %>% summarize(Min_time = min(AirTime, na.rm = T))
