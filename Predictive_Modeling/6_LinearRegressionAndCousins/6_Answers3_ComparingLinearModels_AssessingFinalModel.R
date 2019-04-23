#6.3

library(caret)
library(AppliedPredictiveModeling)

set.seed(0)

# Part (a):
# 
data(ChemicalManufacturingProcess)

processPredictors = ChemicalManufacturingProcess[,2:58]
yield = ChemicalManufacturingProcess[,1]

n_samples = dim(processPredictors)[1]
n_features = dim(processPredictors)[2]

# Part (b): Fill in missing values where we have NAs with the median over the non-NA values: 
#
#gives the median for each column in the data 
replacements = sapply( processPredictors, median, na.rm=TRUE )

for( ci in 1:n_features ){
  bad_inds = is.na( processPredictors[,ci] )
  processPredictors[bad_inds,ci] = replacements[ci]
}


# Look for any features with no variance:
# 
zero_cols = nearZeroVar( processPredictors )
print( sprintf("Found %d zero variance columns from %d",length(zero_cols), dim(processPredictors)[2] ) )
processPredictors = processPredictors[,-zero_cols] # drop these zero variance columns 

# Part (c): Split this data into training and testing sets:
#
training = createDataPartition( yield, p=0.8 )

processPredictors_training = processPredictors[training$Resample1,]
yield_training = yield[training$Resample1]

processPredictors_testing = processPredictors[-training$Resample1,]
yield_testing = yield[-training$Resample1]

# Build some linear models and predict the performance on the testing data set: 
#
set.seed(0)
pls_model = train( processPredictors_training, yield_training, method="pls",
                   # the default tuning grid evaluates components 1 ... tuneLength
                   tuneLength=40, 
                   preProcess=c("center","scale"), trControl=trainControl(method="repeatedcv",repeats=5) )

pls_model
plot(pls_model)
y_hat = predict( pls_model, newdata=processPredictors_testing )
r2_pls = cor(y_hat,yield_testing,method="pearson")^2
rmse_pls = sqrt( mean( (y_hat-yield_testing)^2 ) )
print( sprintf( "%s: Testing R^2= %10.6f; RMSE= %10.6f", "PLS", r2_pls, rmse_pls ) )

# Lets try an Elastic net (this seems to have performed well in past problems) and some other models:
# 
enetGrid = expand.grid(.lambda=seq(0,1,length=20), .fraction=seq(0.05, 1.0, length=20))
set.seed(0)
enet_model = train( processPredictors_training, yield_training, method="enet",
                    # fit the model over many penalty values
                    tuneGrid = enetGrid,
                    preProcess=c("center","scale"), trControl=trainControl(method="repeatedcv",repeats=5) )
saveRDS(enet_model, 'enetmodel_ans_6.3.rds')

y_hat = predict( enet_model, newdata=processPredictors_testing )
r2_enet = cor(y_hat,yield_testing,method="pearson")^2
rmse_enet = sqrt( mean( (y_hat-yield_testing)^2 ) )
print( sprintf( "%-10s: Testing R^2= %10.6f; RMSE= %10.6f", "ENET", r2_enet, rmse_enet ) )

set.seed(0)
lm_model = train( processPredictors_training, yield_training,
                  method="lm", 
                  preProcess=c("center","scale"), 
                  trControl=trainControl(method="repeatedcv",repeats=5) )


y_hat = predict( lm_model, newdata=processPredictors_testing )
r2_lm = cor(y_hat,yield_testing,method="pearson")^2
rmse_lm = sqrt( mean( (y_hat-yield_testing)^2 ) )
print( sprintf( "%-10s: Testing R^2= %10.6f; RMSE= %10.6f", "LM", r2_lm, rmse_lm ) )

# For rlm we cannot have a singular predictor covariance matrix thus we preprocess with PCA:
# 
set.seed(0)
rlm_model = train( processPredictors_training, 
                   yield_training,
                   method="rlm", 
                   preProcess=c("pca"), 
                   trControl=trainControl(method="repeatedcv",repeats=5) )
saveRDS(rlm_model, 'rlmmodel_ans6.2.rds')

y_hat = predict( rlm_model, newdata=processPredictors_testing )
r2_rlm = cor(y_hat,yield_testing,method="pearson")^2
rmse_rlm = sqrt( mean( (y_hat-yield_testing)^2 ) )
print( sprintf( "%-10s: Testing R^2= %10.6f; RMSE= %10.6f", "RLM", r2_rlm, rmse_rlm ) )

# Compare the given models using resamples
#
resamp = resamples( list(pls=pls_model,enet=enet_model,lm=lm_model,rlm=rlm_model) ) # examples of using this are on EPage 82 
print( summary(resamp) )

dotplot( resamp, metric="RMSE" )

print( summary(diff(resamp)) )

# Part (e): Lets look at the coefficients choosen selected by the optimal model:
#
enet_model
plot(enet_model)
library(elasticnet)
enet_base_model = enet( x=as.matrix(processPredictors_training), y=yield_training,
                        lambda=0, normalize=TRUE )
enet_coefficients = predict( enet_base_model, newx=as.matrix(processPredictors_testing),
                             s=0.1, mode="fraction", type="coefficients" )
non_zero = enet_coefficients$coefficients != 0
enet_coefficients$coefficients[ non_zero ]


# Part (f): Explore the relationships between the top predictors and the response:
#
# Pick a predictor and plot how the responce varies as a function of this value
#
p_range = range( processPredictors$ManufacturingProcess32)
variation = seq( from=p_range[1], to=p_range[2], length.out=100 )
mean_predictor_values = apply( processPredictors, 2, mean )


# build a dataframe with variation in only one dimension (for this part we pick ManufacturingProcess32)
library(pracma)

newdata = repmat( as.double(mean_predictor_values), length(variation), 1 )
newdata = data.frame( newdata )
colnames( newdata ) = colnames( processPredictors )
newdata$ManufacturingProcess32 = variation

xs = variation
y_hat = predict( enet_base_model, newx=as.matrix(newdata), s=0.1, mode="fraction", type="fit" )


plot( xs, y_hat$fit, xlab='variation in ManufacturingProcess32', ylab='predicted yield' )
grid()
