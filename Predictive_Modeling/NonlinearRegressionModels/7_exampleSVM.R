set.seed(100)
x <- runif(100, min = 2, max = 10)
y <- sin(x) + rnorm(length(x)) *.25
sinData <- data.frame(x = x, y = y)
plot(x,y, mar = c(1,1,1,1))

dataGrid <- data.frame(epsilon = seq(2,10, length = 100))
 
library(caret)

#radial 
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
grid_radial <- expand.grid(sigma = seq(0,1,0.1),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
ctrl = trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 3)
set.seed(100)
svmRadial <- train(y ~., 
                data = sinData,
                method = 'svmRadial',
                preProc = c('center', 'scale'),
                tuneGrid = grid_radial,
                trControl = ctrl)
svmRadial
plot(svmRadial)

#residuals
rad_pred <- predict(svmRadial)
rad_res <- rad_pred - sinData$y
#predicted vs observed
xyplot(sinData$y ~ rad_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(rad_res ~ rad_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#linear
set.seed(100)
svmLinear <- train(y ~., 
                 data = sinData,
                 method = 'svmLinear',
                 preProc = c('center', 'scale'),
                 tuneGrid = grid,
                 trControl = ctrl)
svmLinear
plot(svmLinear)

#residuals?
lin_pred <- predict(svmLinear)
lin_res <- lin_pred - sinData$y
   #predicted vs observed
xyplot(sinData$y ~ lin_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
   #predicted vs residuals 
xyplot(lin_res ~ lin_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#you can clearly see through looking at the residuals that the radial kernel is better 