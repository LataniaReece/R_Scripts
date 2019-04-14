percent_miss <- function(x) {(sum(is.na(x))/length(x))*100}
rowmiss <- apply(rawdata, 1, percent_miss)
colmiss <- apply(rawdata, 2, percent_miss)

crit_part_miss <- subset(rawdata, rowmiss > 50)
crit_var_miss <- rawdata[colmiss > 50]