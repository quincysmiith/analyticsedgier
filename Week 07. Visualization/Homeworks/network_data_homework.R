#############
#
# Analytics Edge
# Visualizing Network Data Homework
# 
#############

# At work
setwd("C:/Users/marquin.smith/Downloads")

edges <- read.csv("edges.csv")
users <- read.csv("users.csv")

nrow(edges)
nrow(users)

str(edges)
str(users)

nrow(edges) / nrow(users) * 2 

# most common school
table(users$locale)


table(users$locale, users$gender)


install.packages("igraph")
library(igraph)

?graph.data.frame

g <- graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)


# how many connections does each user have
degree(g)

head(edges)


# make size of node relate to number of friends
V(g)$size <- degree(g)/2+2

plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)


# give each gender a different colour
V(g)$color <- "black"
V(g)$color[V(g)$gender == "A"] <- "red"
V(g)$color[V(g)$gender == "B"] <- "gray"

plot(g, vertex.label=NA)


# colour each according to school

V(g)$color <- "black"
V(g)$color[V(g)$school == "A"] <- "red"
V(g)$color[V(g)$school == "AB"] <- "blue"

plot(g, vertex.label=NA)



?igraph.plotting
