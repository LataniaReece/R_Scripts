library(mlbench)
data(Soybean)

str(Soybean)

#PREDICTORS--------------------
#Frequency Distribution
freq_dist <- apply(Soybean[,-1], 2, function(x) table(x))
freq_dist

#Visual Freq Distribution
for (i in names(Soybean[,-1]))
{
  barplot(table(Soybean$i), main = i, ylim = c(0,700), axes = FALSE)
  axis(side = 2, at = seq(0, 700, 75))
}

#Missing Data--------------
percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(Soybean, 1, percent_miss)
colmiss <- apply(Soybean, 2, percent_miss)

missing <- subset(Soybean, rowmiss > 0)

#checking if a certain outcome is related to missing variable
table(missing$Class)

#further exploration of observations with missing values 
Soybean$missing <- ifelse(rownames(Soybean) %in% rownames(missing), "NAs", "No NAs")
table(Soybean$missing)

missing_data <- Soybean[Soybean$missing == 'NAs',]
colmiss <- apply(missing_data, 2, percent_miss)
colmiss <- data.frame(variables = names(colmiss), missing_percentage = colmiss, row.names = NULL)

colmiss[colmiss$missing_percentage > 80,]
nrow(colmiss[colmiss$missing_percentage > 80,])

#in this case I would probably rmeove the missing values...they are missing 11 variables
#or, remove them and do machine learning? and see if those variable are even important in the first place?
#or see if the classes they are in should just be remove period?

table(Soybean$Class)
table(missing$Class)

#for example: all of the obs in 2-4-d-injury  diaporthe-pod-&-stem-blight cyst-nematode are missing
#data while 68 phytophthora-rot of the 88 in total are missing data 
#would probably just remove these classes?


