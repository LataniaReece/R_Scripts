#Data-----------------
rawdata <- read.csv("Admission_Predict.csv")
library(textclean)
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)

#Fixing Factor Variables
rawdata <- rawdata %>% 
  mutate(sop_g5 = sop, lor_g5 = lor) %>%
  select(-c(serialno, sop, lor))
rawdata[,c(3,5,7:8)] <- lapply(rawdata[,c(3,5,7:8)], factor)
levels(rawdata$sop_g5) <- rep(1:5, each = 2)
levels(rawdata$lor_g5) <- rep(1:5, each = 2)

str(rawdata)