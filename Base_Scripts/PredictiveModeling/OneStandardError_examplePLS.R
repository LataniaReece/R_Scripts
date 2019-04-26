#PLS Model
pls_grid <- expand.grid(ncomp = 1:10)
ctrl <- trainControl(method = 'cv',
                     number = 10)
set.seed(10)
plsModel <- train(Yield ~.,
                  data = training,
                  method = 'pls',
                  tuneGrid = pls_grid,
                  trControl = ctrl)

plsModel
plsModel$results

#RMSE error bars - simpler model lower than red line is good (low RMSE = good)
rmse_comp <- plsModel$results$ncomp
rmse_means <- plsModel$results$RMSE
rmse_std_errors <- plsModel$results$RMSESD

rmse_comp

errbar(rmse_comp, rmse_means, rmse_means+rmse_std_errors, rmse_means-rmse_std_errors )  
grid()
min_index = which.min(rmse_means)
abline(h=rmse_means[min_index] + rmse_std_errors[min_index], col='red' )


#R2 error bars - simpler model greater than the red line is good (higher R2 = better)
r2_comp <- plsModel$results$ncomp
r2_means <- plsModel$results$Rsquared
r2_std_errors <- plsModel$results$RsquaredSD

errbar(r2_comp, r2_means, r2_means+r2_std_errors, r2_means-r2_std_errors )  
grid()
max_index = which.max(r2_means)
abline(h=r2_means[max_index] - r2_std_errors[max_index], col='red' )