#url: https://youtu.be/5eDqRysaico

# Cluster Analysis
data(iris)
#for this demo just going to use Petal.Length and Petal.Width
data <- data.frame(cbind(petal.length = iris$Petal.Length, petal.width = iris$Petal.Width))
# Scatter plot 
plot(data$petal.length ~ data$petal.width, data = data)

# Normalize 
library(robustHD)
norm_data <- standardize(data)

##calculate distance matrix (default is Euclidean distance)
distance <- dist(norm_data)

# Hierarchical agglomerative clustering using default complete linkage 
data.hclust = hclust(distance)
plot(data.hclust)

# Hierarchical agglomerative clustering using "average" linkage 
data.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)

# Cluster membership - shows how many are in each cluster
member <- cutree(data.hclust, 3)
table(member)

#Characterizing clusters 
aggregate(norm_data,list(member),mean)
aggregate(data,list(member),mean) #shows mean in original units

#Silhouette Plot 
library(cluster)
plot(silhouette(cutree(data.hclust,3),distance))

# Scree Plot - overall we want to reduce within cluster variability (within group SS)
#"elbow method'
wss <- (nrow(norm_data)-1)*sum(apply(norm_data,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(norm_data, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 


# K-means clustering
set.seed(12)
kc <- kmeans(norm_data, 3, iter.max = 100)
kc
plot(petal.length~petal.width, data,  col = kc$cluster)
