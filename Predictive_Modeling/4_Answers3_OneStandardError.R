save_plots = F

library(Hmisc)

library(AppliedPredictiveModeling)
data(ChemicalManufacturingProcess)
data <- ChemicalManufacturingProcess

#Missing Data
apply(data, 2, function(x) {sum(is.na(x))})
sum(apply(data, 2, function(x) {sum(is.na(x))}))
#Impute missing
library(mice)
grep("Yield", names(data))
impute <- mice(data[,-1], m = 3, seed = 123)
new_data <- complete(impute, 2)
apply(new_data, 2, function(x) {sum(is.na(x))})
sum(apply(new_data, 2, function(x) {sum(is.na(x))}))

#Data Partition 
new_data <- cbind(new_data, Yield = data$Yield)
str(new_data)
set.seed(10)
inTrain <- createDataPartition(new_data$Yield,
                               p = 0.7,
                               list = FALSE)
training <- new_data[inTrain,]
testing <- new_data[-inTrain,]

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



# # Get the given data into a form we can plot:
# #
# components = 1:10
# means = c( 0.444, 0.500, 0.533, 0.545, 0.542, 0.537, 0.534, 0.534, 0.520, 0.507 )
# std_errors = c( 0.0272, 0.0298, 0.0302, 0.0308, 0.0322, 0.0327, 0.0333, 0.0330, 0.0326, 0.0324 )
# data = data.frame( components, means, std_errors ) 
# 
# if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter4/chap_4_prob_3_R2_plot.eps", onefile=FALSE, horizontal=FALSE) }
# errbar( components, means, means+std_errors, means-std_errors )  
# grid()
# max_index = which.max( means )
# abline( h=means[max_index] - std_errors[max_index], col='red' )
# if( save_plots ){ dev.off() }
