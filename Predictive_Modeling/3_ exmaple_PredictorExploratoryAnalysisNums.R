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

#explore observations with missing values 
Soybean$missing <- ifelse(rownames(Soybean) %in% rownames(missing), "NAs", "No NAs")
table(Soybean$missing)

missing_data <- Soybean[Soybean$missing == 'NAs',]
colmiss <- apply(missing_data, 2, percent_miss)
colmiss <- data.frame(variables = names(colmiss), missing_percentage = colmiss, row.names = NULL)

colmiss[colmiss$missing_percentage > 80,]

