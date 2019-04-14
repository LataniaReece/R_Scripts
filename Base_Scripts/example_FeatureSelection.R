

#Data-------------
rawdata<- read.csv("Admission_Predict.csv")
library(textclean)
names(rawdata) <- strip(names(rawdata), lower.case = TRUE)
rawdata$serialno <- NULL
#Adding Random variables
variables <- list()
for i in 1:30
{
  
  
}
rawdata$rand1 <- sample(c(1:4), 400, replace=TRUE, prob=c(0.25, 0.25, 0.25, 0.25))
rawdata$rand2 <- sample(c(1:300), 400, replace = TRUE)

#Filter Feature Selection-----------------------
library(caret)
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
filter_sel <- sbf(x = rawdata[,-8], y = rawdata$chanceofadmit, sbfControl = filterCtrl )
filter_sel

filter_sel$variables
#suggested to keep
# During resampling, the top 5 selected variables (out of a possible 7):
#   cgpa (100%), grescore (100%), lor (100%), research (100%), sop (100%)

#Recursive Feature Selection------------------
ctrl <- rfeControl(functions = lmFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
set.seed(10)
lmProfile <- rfe(x = rawdata[,-8], 
                 y = rawdata$chanceofadmit,
                 rfeControl = ctrl)
lmProfile
#suggested to keep: 
# The top 5 variables (out of 8):
#   cgpa, research, lor, universityrating, sop

plot(lmProfile, type = c("g", "o"))
lmProfile$fit
head(lmProfile$resample)
