#Loading RDS Models
enetModel <- readRDS('')
marsModel <- readRDS('')
svmRModel <- readRDS('')
nnetModel <- readRDS('')
rpartModel <- readRDS('')
ctreeModel <- readRDS('')
mtModel <- readRDS('')
treebagModel <- readRDS('')
rfModel <- readRDS('')
gbmModel <- readRDS('')
cbModel <- readRDS('')

#Resamples
allResamples <- resamples(list('Linear Reg' = lmModel,
                               'PLS' = plsModel,
                               'Elastic Net' = enetModel,
                               'MARS' = marsModel,
                               'SVM' = svmRModel,
                               'Neural Networks' = nnetModel,
                               'CART' = rpartModel,
                               'Cond Inf Tree' = ctreeModel,
                               'Bagged Tree' = treebagModel,
                               'Boosted Tree' = gbmModel, 
                               'Random Forest' = rfModel,
                               "Cubist" = cbModel))
