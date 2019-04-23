#Ans 7.1

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

#poly kernel - same line as the linear one
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'polydot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'red')

#radial kernel
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#tweaking C, sigma and epsilon
#lowering C - should overfit
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 10^2, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'red')

#increasing C - should underfit
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 10^(-2), epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'green')

#epsilon
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
               C = 1, epsilon = 1.8)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'red')

#decreasing epsilon
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.5)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'green')

#original
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = 'automatic',
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
plot(x,y, mar = c(1,1,1,1))
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'blue')

#increasing sigma - overfits 
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = list(sigma = 100),
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'red')

#decreasing signma
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel = 'rbfdot', kpar = list(sigma = 0.1),
               C = 1, epsilon = 0.1)
pred <- predict(rbfSVM, newdata = dataGrid)
points(x = dataGrid$x, y = pred[,1],
       type = 'l', col = 'green')

