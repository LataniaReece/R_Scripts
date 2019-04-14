x <- rgamma(50, 3, .5)
y <- rnorm(50)

library(caret)
trainIndex = createDataPartition(df$y,
                                 p=0.5, list=FALSE,times=1)

trainIndex

train = df[trainIndex,]
test = df[-trainIndex,]
