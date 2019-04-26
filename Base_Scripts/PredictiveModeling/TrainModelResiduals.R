lmModel
lmTraining_pred <- predict(lmModel, solTrainXtrans)
lmTraining_residuals <- lmTraining_pred - solTrainY
#predicted vs observed 
plot(lmTraining_pred, solTrainY, 
     xlab = 'Predicted',
     ylab = 'Observed',
     main = 'Predicted vs Observed')
abline(0,1, col = 'red')
#Predicted vs residuals 
plot(lmTraining_pred, lmTraining_residuals, 
     xlab = 'Predicted',
     ylab = 'Residuals',
     main = 'Predicted vs Residuals')
abline(h = 0, col = 'red')