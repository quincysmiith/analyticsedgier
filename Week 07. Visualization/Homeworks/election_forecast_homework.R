#############
#
# Analytics Edge
# Election Forecast Homework
#
#############

library(ggplot2)
library(maps)
library(ggmap)

statesMap <- map_data("state")
str(statesMap)

table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
