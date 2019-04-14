# Using prcomp and varimax for PCA in R www.youtube.com/watch?v=PSuvMBtvJcA 
#2012

library(lattice)
url <- "https://raw.githubusercontent.com/steviep42/youtube/master/YOUTUBE.DIR/wines.csv"
wines <- read.csv(url, header=TRUE)


#Look at correlation: His method: 
#Darker colors = stronger relationship
library(gclus)
my.abs     <- abs(cor(wines[,-1]))
my.colors  <- dmat.color(my.abs)
my.ordered <- order.single(cor(wines[,-1]))
cpairs(wines, my.ordered, panel.colors=my.colors, gap=0.5)


#Look at correlaton: featureplot method
library(caret); library(ggplot2)
featurePlot(x = wines[,-1],
            y=wines[,1])

#Do the PCA 
my.prc <- prcomp(wines[,-1], center=TRUE, scale=TRUE)
summary(my.prc) # you see that the first 3 principal components capture 0.98937 of the variance

#Which PC's do I keep? (2 methods)

#Method 1, Eigen values greater than 1
my.prc$sdev ^ 2 # gives you teh eigenvalues, in this case only 1 and 2 are greater than one

#Method 2, Look at Scree Plot
screeplot(my.prc, main="Scree Plot", xlab="Components")
screeplot(my.prc, main="Scree Plot", type="line" )

#Dotplot PC1 # shows which variables have the most loadings on PC1
load    <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

#Dotplot PC2 #shows which variables have the most loadings on PC2
sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2"
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

#biplot
biplot(my.prc, cex=c(1, 0.7))
abline(h=0, v =0)

# Apply the Varimax Rotation
my.var <- varimax(my.prc$rotation) #emphasizes which variables have a greater loading on each principal component
my.var


