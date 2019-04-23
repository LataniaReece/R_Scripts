percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(rawdata, 1, percent_miss)
colmiss <- apply(rawdata, 2, percent_miss)

crit_obs_miss <- subset(rawdata, rowmiss > 50)
crit_var_miss <- rawdata[colmiss > 50]

nrow(crit_obs_miss)
length(crit_var_miss)


#predictive Modeling: checking how much missing in each variable: 
# Count how many NA's we have in each feature:
#
library(mlbench)
data(Soybean)

apply( Soybean, 2, function(x){ sum(is.na(x)) } )

#percentage
apply( Soybean, 2,function(x) {(sum(is.na(x))/length(x))*100} )