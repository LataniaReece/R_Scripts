set.seed(100)
x <- runif(100, min = 2, max = 10)
y <- sin(x) + rnorm(length(x)) * .25
sinData <- data.frame(x = x, y = y)
plot(x,y, mar = c(1,1,1,1))

#testing set 
dataGrid <- data.frame(x = seq(2,10, length = 100))

library(kernlab)
#linear kernel
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'vanilladot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#poly kernel
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'polydot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#radial kernel
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#tweaking C, sigma and epsilon
#lowering C
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 0.01, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing C
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 0.5, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing C
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing epsilon
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing epsilon
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 1.5)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing epsilon 
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 1.5)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#decreasing epsilon
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.5)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#decreasing epsilon
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = -1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#original
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#sigma
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = list(sigma = 1),
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing sigma 
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = list(sigma = 100),
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#decreasing signma
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = list(sigma = 0.1),
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#decreasing sigma
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = list(sigma = 0),
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#I feel like cost and sigma still maintain the shape of the relationship 
#( except when sigma is 0  negative)
#epsilon overall messes with the shape 


