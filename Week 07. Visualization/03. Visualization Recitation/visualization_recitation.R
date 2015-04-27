#############
#
# Analytics Edge
# Visualization recitation walkthrough
#
#############

# At work
setwd("C:/Users/marquin.smith/Downloads")


### Bar charts in R

library(ggplot2)

intl <- read.csv("intl.csv")
str(intl)

ggplot(intl, aes(x = Region, y= PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))


# put bars in descending order
intl <- transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

# change from decimal place to whole numbers
intl$PercentOfIntl <- intl$PercentOfIntl *100

ggplot(intl, aes(x = Region, y= PercentOfIntl)) +
    geom_bar(stat="identity", fill="dark blue") + 
    geom_text(aes(label=PercentOfIntl), vjust = -0.4) + 
    ylab("Percent of international Sudents") + 
    theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


### World Maps in R

library(ggmap)

intlall <- read.csv("intlall.csv", stringsAsFactors=FALSE)
head(intlall)

intlall[is.na(intlall)] <- 0
head(intlall)

world_map <- map_data("world")

str(world_map)


world_map <- merge(world_map, intlall, by.x="region", by.y="Citizenship")

str(world_map)

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="white", color="black") + 
    coord_map("mercator")

# we need to reorder the maps data to make the maps a proper shape
# the maps data is basically a list of coordinates outlining the borders of each country

world_map <- world_map[order(world_map$group, world_map$order), ]

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="white", color="black") + 
    coord_map("mercator")

# the reason china is missing is beceause it has i different name in the intall
# data as it does in the world_map data so it wasn't picked up in the merge

table(intlall$Citizenship)

intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"

table(intlall$Citizenship)

world_map <- merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")
world_map <- world_map[order(world_map$group, world_map$order), ]

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="white", color="black") + 
    coord_map("mercator")

# fill based on porportion of students
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=Total), color="black") + 
    coord_map("mercator")

# authographic projection
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=Total), color="black") + 
    coord_map("ortho", orientation=c(20,30,0))

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=Total), color="black") + 
    coord_map("ortho", orientation=c(-37,175,0))


### Using Line charts instead

library(ggplot2)

households <- read.csv("households.csv")
str(households)

library(reshape2)

#Look at first 2 columns
households[,1:2]

head(melt(households, id="Year"))


households[,1:3]

melt(households, id="Year")[1:10,]
nrow(melt(households, id="Year"))

# we can now plot this
ggplot(melt(households, id="Year"), aes(x=Year, y=value, color = variable)) +
    geom_line(size=2) + geom_point(size = 5) + ylab("Percentage of households")