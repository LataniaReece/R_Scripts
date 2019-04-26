library(caret) # to get BoxCoxTrans
Glass$Mg = Glass$Mg + 1.e-6 # add a small value so that BoxCoxTransfs will converge 
Glass$K = Glass$K + 1.e-6
Glass$Ba = Glass$Ba + 1.e-6
Glass$Fe = Glass$Fe + 1.e-6

boxcox_skewness = function(x){
  BCT = BoxCoxTrans(x)
  x_bc = predict( BCT, x )
  skewness(x_bc) 
}

apply( Glass[,-10], 2, boxcox_skewness )


#May way of getting the variables
length(Glass[,-10])

i <- 1
while(i<10){
  boxcox_skewness = function(x){
    BCT = BoxCoxTrans(x)
    x_bc = predict( BCT, x )}
  i = i+1
}

check <- apply( Glass[,-10], 2, boxcox_skewness )
check <- data.frame(check, Type = Glass$Type)
head(check)

apply( check[,-10], 2, skewness )