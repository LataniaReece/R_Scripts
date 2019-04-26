#linsvm residuals--------------------
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

#radialsvm residuals-------------------------------
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

#knn residuals
knn_pred <- predict(knnModel)
knn_res <- knn_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ knn_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(knn_res ~ knn_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#mars residuals-------------------------------
mars_pred <- predict(marsModel)
mars_res <- mars_pred - sinData$y
#predicted vs observed
xyplot(sinData$y ~ mars_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(mars_res ~ mars_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

#nnet residuals-------------------------------
nnet_pred <- predict(nnetModel)
nnet_res <- nnet_pred - training$fat
#predicted vs observed
xyplot(training$fat ~ nnet_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Observed',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(0,1, col = 'red')
       })
#predicted vs residuals 
xyplot(nnet_res ~ nnet_pred,
       type = c('p', 'g'),
       xlab = 'Predicted', ylab = 'Residuals',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, col = 'red')
       })

