library(plyr)
library(dplyr)
library(ggplot2)

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