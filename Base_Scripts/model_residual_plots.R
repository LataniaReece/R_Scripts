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