#############
#
# Analytics Edge
# Segmenting Images to create data
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 06. Clustering/03. Segmenting Images to create data")

### Clustering Pixels

flower <- read.csv("flower.csv", header=FALSE)

str(flower)

flowerMatrix <- as.matrix(flower)
str(flowerMatrix)


# make into vector
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

# calculate distance matrix for hierarchical clustering
distance <- dist(flowerVector, method = "euclidean")

# create heirarchical clustering
clusterIntensity <- hclust(distance, method="ward.D")
plot(clusterIntensity)

# see where the cut off is when choosing the number of clusters
rect.hclust(clusterIntensity, k=3, border="red")


flowerClusters <- cutree(clusterIntensity, k=3)
flowerClusters

tapply(flowerVector, flowerClusters, mean)

# See what patterns our clusters made
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes=FALSE)

# see original image
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

### MRI image

healthy <- read.csv("healthy.csv", header = FALSE)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)

# take a look at the image
image(healthyMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))


healthyVector <- as.vector(healthyMatrix)
distance <- dist(healthyVector, method = "euclidean")

# Produces error message
# "Error: cannot allocate vector of size 498.0 Gb"

str(healthyVector)

# this would need to store 66 billion values to be able to do hierarchical clustering
# Is there another way?

# try k-means clustering instead

### K-means clustering

k = 5

set.seed(1)

KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyClusters <- KMC$cluster

KMC$centers

# change healthyClusters to a matric

dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col=rainbow(k))

# Can this be used to find tumours in unhealthy brains?


### Detecting Tumors

tumor <- read.csv("tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

# need an extra package to treat healthy brain as training set and tumor brain as test set

install.packages("flexclust")

library("flexclust")

KMC_kcca <- as.kcca(KMC, healthyVector)

tumorClusters <- predict(KMC_kcca, newdata=tumorVector)

dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))
