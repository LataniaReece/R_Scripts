library(caret)

data(oil)

set.seed(0)

str(oilType)
table(oilType)

# What is the population frequency of oil types:
#
table(oilType)/length(oilType)

# Draw a sample:
# 
a_sample = sample( oilType, 60, replace=TRUE )

# How do its frequencies compare with that of the population:
# 
table(a_sample)/length(a_sample)
another_sample = createDataPartition( oilType, p=0.625 )
table(oilType[ another_sample$Resample1 ])/length(another_sample$Resample1)


#p 92
# To obtain a confidence interval for the overall accuracy,
# the based R function binom.test can be used.
# It requires the user to input the number of samples and the number correctly classified to 
# calculate the interval. For example, suppose a test set sample of 20 oil samples was set aside 
# and 76 were used for model training. For this test set size and a model that is about 80% 
# accurate (16 out of 20 correct), the confidence interval would be computed using:
binom.test(16, 20) 
#In this case, the width of the 95% connfidence interval is 37.9%.

num_correct = 16:20
width_of_interval = c()
for( nc in num_correct ){
  bt_out = binom.test( nc, 20 )
  width_of_interval = c( width_of_interval, diff( bt_out$conf.int ) )
}

plot( num_correct, width_of_interval, type='l', xlab='number of correct samples (from 20)', ylab='width of 95% confidenci interval' )
grid() 

#shows that as the number of samples you get are correct, then you decrease the confidence interval
#which means that your uncertainty of accuracy decreases(which is good)




