################################################################################
# Analyzing clusters with silhouettes
################################################################################
#install.packages('cluster')
library(cluster)
species <- which(names(iris) =='Species')
iris.dist <- dist(iris[,-species])


iris.cluster.3 <- kmeans(iris[,-species], 3, nstart=10)
iris.cluster.4 <- kmeans(iris[,-species], 4, nstart=10)
iris.cluster.9 <- kmeans(iris[,-species], 9, nstart=10)

# silhouettes indicating self-similar each cluster is relative to
# the closest neighbouring cluster
silhouette.3 <- silhouette(iris.cluster.3$cluster, iris.dist)
silhouette.4 <- silhouette(iris.cluster.4$cluster, iris.dist)
silhouette.9 <- silhouette(iris.cluster.9$cluster, iris.dist)

plot(silhouette.3)
plot(silhouette.4)
plot(silhouette.9)

################################################################################
# plotting data points based on clusters and geography
################################################################################
# install.packages('GISTools') 
library(GISTools)
library(maps)
library(RColorBrewer)
ling.data <- read.table('binary-ling-data.data', header=T)
ling.responses <- ling.data[,7:ncol(ling.data)]

# subsample the dataset and cluster
k <- 3
set.seed(47)
samp.size <- 5000
sample <- list()
sample$idcs <- sample(1:nrow(ling.data), samp.size)
sample$coords <- ling.data[sample$idcs, c('long', 'lat')]
sample$responses  <- ling.responses[sample$idcs,]
sample$kmeans <- kmeans(sample$responses, k)

colors <- brewer.pal(k, 'Set1')
colors <-  add.alpha(colors, alpha=0.5) # add transparency
map('state', col='black', fill=F)
points(sample$coords, col=colors[sample$kmeans$cluster], pch=20, cex=.75)



