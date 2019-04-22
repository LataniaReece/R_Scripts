#comparing models 
models <- list(MARS = marsModel, 
               LinearSVM = svmLinear,
               RadialSVM = svmRadial,
               NeuralNetwork = nnetModel,
               KNN = knnModel)
resamps <- resamples(models)
summary(resamps)
