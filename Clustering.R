### Clean RAM
rm(list = ls())
gc()

### Set Random Seed
set.seed(1234)

##########################
### K-Means Clustering ###
##########################

(kmeans.clust <- kmeans(iris[,1:4], 3)) 
attributes(kmeans.clust)
kmeans.clust$cluster
kmeans.clust$centers
kmeans.clust$size
kmeans.clust$iter
kmeans.clust$ifault
kmeans.clust$totss
kmeans.clust$betweenss
kmeans.clust$withinss
kmeans.clust$tot.withinss

### Visualize Centers
table(iris$Species, kmeans.clust$cluster)
plot(iris[,c("Sepal.Length", "Sepal.Width")], col = kmeans.clust$cluster)
points(kmeans.clust$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = "#", cex=2.5)

##########################################
### K-Medoids Clustering - Using PAM ####
##########################################

# PAM - Partitioning Around Mediods

library(cluster)
(pam.clust <- pam(iris[,1:4], 3))
attributes(pam.clust)

pam.clust$medoids
pam.clust$id.med
pam.clust$clustering
pam.clust$objective
pam.clust$isolation
pam.clust$clusinfo
pam.clust$silinfo
pam.clust$diss
pam.clust$call
pam.clust$data

### Visualize Mediods
table(iris$Species, pam.clust$clustering)
plot(iris[,c("Sepal.Length", "Sepal.Width")], col = pam.clust$clustering)
points(pam.clust$medoids[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = "X", cex=2.5)

### ClustPlot And Silhouette Plot
plot(pam.clust)

##########################################
### K-Medoids Clustering - Using PAMK ####
##########################################

# PAMK - Partitioning Around Mediods With Estimation of Number of Clusters

library(fpc)
(pamk.clust <- pamk(iris[,1:4]))
attributes(pamk.clust)

pamk.clust$pamobject
pamk.clust$nc
pamk.clust$crit

### Visualize Mediods
table(iris$Species, pamk.clust$pamobject$clustering)
plot(iris[,c("Sepal.Length", "Sepal.Width")], col = pamk.clust$pamobject$clustering)
points(pamk.clust$pamobject$medoids[,c("Sepal.Length", "Sepal.Width")], col = 1:2, pch = "O", cex=2.5)

### ClustPlot And Silhouette Plot
plot(pamk.clust$pamobject)

###############################################
### Hierarchical Clustering - Using HCLUST ####
###############################################
set.seed(2835)
(hier.clust <- hclust(dist(iris[,1:4]), method = "ave", members = NULL))

hier.clust$dist.method
hier.clust$call
hier.clust$method
hier.clust$labels
hier.clust$order
hier.clust$height
hier.clust$merge

print(hier.clust)
plot(hier.clust)
plot(hier.clust, hang = -1, labels=iris$Species)

# Identify Clusters
identify(hier.clust)
rect.hclust(hier.clust, k=3)
rect.hclust(hier.clust, h=1.8)

# Cut Tree and Group Clusters
iris.clust <- cutree(hier.clust, k=3)
plot(iris.clust)

################################################
### Density Based Clustering - Using DBSCAN ####
################################################

library(fpc)
(dbscan.clust <- dbscan(iris[,1:4], eps = 0.42, MinPts = 5, scale = FALSE))

dbscan.clust$cluster
dbscan.clust$eps
dbscan.clust$MinPts
dbscan.clust$isseed

print(dbscan.clust)
plot(dbscan.clust, iris[,1:4])
plotcluster(iris[,1:4], dbscan.clust$cluster)

# compare clusters with original class labels
table(dbscan.clust$cluster, iris$Species)

# Predict new cluster
pred.clust <- predict(dbscan.clust, iris[,1:4], iris[,1:4])
table(pred.clust, iris$Species)