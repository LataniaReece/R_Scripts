library(mlbench)
data(Soybean)

# See if a class has more NA's than others:
#
Soybean$has_nans_in_sample = apply( Soybean[,-1], 1, function(x){ sum(is.na(x)) > 0 } )
#variable - 'has_nans_in_sample' is now variable # 34
table( Soybean[, c(1,34) ] )
names(Soybean)