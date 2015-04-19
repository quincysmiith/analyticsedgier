#############
#
# Analytics Edge
# Introduction to clustering
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 06. Clustering/01. Intro to Clustering")

movies <- read.table("movieLens.txt", header=FALSE, sep = "|", quote="\"")
str(movies)

colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unkown", "Action",
                      "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary",
                      "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance",
                      "SciFy", "Thriller", "War", "Western")

str(movies)

# Remove columns we won't use

movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL

movies <- unique(movies)
str(movies)


#
# Quick Question
#

table(movies$Comedy)

table(movies$Western)

table(movies$Romance, movies$Drama)


### Hierarchical Clustering in R

distances <- dist(movies[2:20], method="euclidean")

# Create clusters
clusterMovies <- hclust(distances, method="ward.D")
plot(clusterMovies)

# assign each data point into a cluster
clusterGroups <- cutree(clusterMovies, k = 10)

# find the proportion of movies in each group labelled as action
tapply(movies$Action, clusterGroups, mean)

# this can be applied to other genres such as Romance
tapply(movies$Romance, clusterGroups, mean)

# See if we can find what cluster Men in Black is in
subset(movies, Title == "Men in Black (1997)")

# this 257th row
clusterGroups[257]


cluster2 <- subset(movies, clusterGroups==2)
cluster2$Title[1:10]

#
# Quick Question
#

clusterGroupsTest <- cutree(clusterMovies, k = 2)

group1 <- subset(movies, clusterGroupsTest==1)
group2 <- subset(movies, clusterGroupsTest==2)

head(group2)
