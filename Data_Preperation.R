#############################################
#### Free Memory/ Garbage Collection ########
#############################################
rm(list = ls())
gc()

###############################
#### Outlier Detection ########
###############################

# Normal Distribution
set.seed(4321)
norm.dist <- rnorm(100)
summary(norm.dist)
hist(norm.dist)
plot(norm.dist)

# detect outliers
(boxplot(norm.dist))
(boxplot(norm.dist))$stats
(boxplot(norm.dist))$out

boxplot.default(norm.dist)
boxplot.stats(norm.dist)
boxplot.stats(norm.dist)$out

# Uniform Distribution
set.seed(1234)
unif.dist <- runif(100)
summary(unif.dist)
hist(unif.dist)
plot(unif.dist)

# detect outliers
(boxplot(unif.dist))
(boxplot(unif.dist))$stats
(boxplot(unif.dist))$out

boxplot.default(unif.dist)
boxplot.stats(unif.dist)
boxplot.stats(unif.dist)$out

# Data With Outliers
test.out <- c(-100:-95, -40:40, 95:100)
summary(test.out)
hist(test.out)
plot(test.out)

# detect outliers
(boxplot(test.out))
(boxplot(test.out))$stats
(boxplot(test.out))$out

boxplot.default(test.out)
boxplot.stats(test.out)
boxplot.stats(test.out)$out


###############################
#### Outlier Removal ##########
###############################

# Remove Outlier Observations from test dataset

x.out <- c(-100:-95, -40:40)
summary(x.out)
hist(x.out)
plot(x.out)

y.out <- c(100:97, -40:40, 95, 96)
summary(y.out)
hist(y.out)
plot(y.out)

df.out <- data.frame(x.out, y.out)

(x.ind.out <- which(x.out %in% boxplot.stats(x.out)$out))
(y.ind.out <- which(y.out %in% boxplot.stats(y.out)$out))

# common outlier indices in both variables
(common_out <- intersect(x.ind.out, y.ind.out))
plot(df.out)
points(df.out[common_out,], col="blue", pch="*", cex=2.5)

# All outlier indices in both variables
(all_out <- union(x.ind.out, y.ind.out))
plot(df.out)
points(df.out[all_out,], col="red", pch="+", cex=2.5)


# Remove Outlier Observations from IRIS dataset

(Sepal.Length.ind.out <- which(iris$Sepal.Length %in% boxplot.stats(iris$Sepal.Length)$out))
(Sepal.Width.ind.out <- which(iris$Sepal.Width %in% boxplot.stats(iris$Sepal.Width)$out))
(Petal.Length.ind.out <- which(iris$Petal.Length %in% boxplot.stats(iris$Petal.Length)$out))
(Petal.Width.ind.out <- which(iris$Petal.Width %in% boxplot.stats(iris$Petal.Width)$out))

# All outlier indices in both variables
#(all_out <- union(Sepal.Length.ind.out, Sepal.Width.ind.out, Petal.Length.ind.out, Petal.Width.ind.out))
#print(iris[all_out,])
print(iris[Sepal.Width.ind.out,])
plot(iris[, c('Sepal.Length', 'Sepal.Width')])
points(iris[Sepal.Width.ind.out, c('Sepal.Length', 'Sepal.Width')], col="red", pch="+", cex=2.5)

# Remove Outlier Observations from IRIS dataset using LOF Algorithm 

library(DMwR)
lof.scores <- lofactor(subset(iris, select = -Species), k=5)
iris.ind.out <- order(lof.scores, decreasing=TRUE)[1:4]

print(iris[iris.ind.out,])
plot(iris[, c('Sepal.Length', 'Sepal.Width')])
points(iris[iris.ind.out, c('Sepal.Length', 'Sepal.Width')], col="blue", pch="*", cex=2.5)

library(Rlof)
Rlof.scores <- lof(subset(iris, select = -Species), k=5)
iris.ind.out <- order(Rlof.scores, decreasing=TRUE)[1:4]

print(iris[iris.ind.out,])
plot(iris[, c('Sepal.Length', 'Sepal.Width')])
points(iris[iris.ind.out, c('Sepal.Length', 'Sepal.Width')], col="orange", pch="o", cex=2.5)

# Remove Outlier Observations from IRIS dataset using KMeans Algorithm
kmeans.result <- kmeans(iris[,1:4], centers=3)
kmeans.result$centers
kmeans.result$cluster

# calculate distances between observations and cluster centers
clust.centers <- kmeans.result$centers[kmeans.result$cluster, ]
euc.dist <- sqrt(rowSums((iris[,1:4] - clust.centers)^2))

# pick top 4 largest distances
iris.ind.out <- order(euc.dist, decreasing=TRUE)[1:4]
print(iris[iris.ind.out,])
plot(iris[, c('Sepal.Length', 'Sepal.Width')])
points(iris[iris.ind.out, c('Sepal.Length', 'Sepal.Width')], col="green", pch="#", cex=2.5)

plot(iris[,c("Sepal.Length", "Sepal.Width")], col=kmeans.result$cluster, pch="+", cex=0.5)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch="*", cex=2.5)
points(iris[iris.ind.out, c("Sepal.Length", "Sepal.Width")], pch="o", col=4, cex=2.5)

###############################
#### PCA Analysis #############
###############################

iris.pca <- prcomp(iris[,1:4], retx = TRUE, center = TRUE, scale. = TRUE)
plot(iris.pca$x)
biplot(iris.pca)
biplot(iris.pca, xlabs=c(rep(".", nrow(iris.pca$x))))

labels[-iris.ind.out] <- "+"
labels[iris.ind.out] <- "o"
biplot(iris.pca, cex=1.0, xlabs=labels)

pairs(iris.pca$x)

