data(iris)
str(iris)

pca <- prcomp(iris[,-5], scale = TRUE)

plot(pca$x[,1], pca$x[,2]) 
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

#plotting the first 2 components - there should be no correlation
barplot(pca.var.per[1:15], main = 'Scree Plot',
        xlab = 'Principal Component',
        ylab = 'Percent Variation', 
        ylim = c(0, 100))

pca_absorp$rotation[,1:2]

pca.data <- data.frame(Sample = (1:nrow(iris)),
                       PC1 = pca$x[,1],
                       PC2 = pca$x[,2],
                       Species = iris$Species)

#to see which samples are where 
library(ggplot2)
ggplot(data = pca.data, aes(x = PC1, y = PC2, label = Sample, color = Species))+
  geom_text()+
  xlab(paste('PC1 -', pca.var.per[1], '%', sep = ""))+
  ylab(paste('PC2 -', pca.var.per[2], '%', sep = ""))+
  theme_bw()+
  ggtitle("My PCA Graph")

ggplot(data = pca.data, aes(x = PC1, y = PC2, label = Sample))+
  geom_text()+
  xlab(paste('PC1 -', pca.var.per[1], '%', sep = ""))+
  ylab(paste('PC2 -', pca.var.per[2], '%', sep = ""))+
  theme_bw()+
  ggtitle("My PCA Graph")

#loading scores for PC1
loading_scores <- pca$rotation[,1]
variable_scores <- abs(loading_scores)
variable_scores_ranked <- sort(variable_scores, decreasing = TRUE)
variable_scores_ranked
