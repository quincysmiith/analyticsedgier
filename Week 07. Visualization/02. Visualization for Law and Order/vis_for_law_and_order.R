#############
#
# Analytics Edge
# Visualization for law and order walkthrough
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 07. Visualization/02. Visualization for Law and Order")

mvt <- read.csv("mvt.csv", stringsAsFactors=FALSE)

mvt$Date <- strptime(mvt$Date, format="%m/%d/%y %H:%M")

# Add weekday and hour to data
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour<- format(mvt$Date, "%H")

str(mvt)

table(mvt$Weekday)

# convert table to dataframe for plotting
WeekdayCounts <- as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

library(ggplot2)

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
# This plotted in alphabetical order

WeekdayCounts$Var1 <- factor(WeekdayCounts$Var1, ordered = TRUE, 
                             levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))

# Add useful labels
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1)) + 
    xlab("Day of the Week") + ylab("Total Motor Vehicle thefts")

#
# Quick Question
#

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype=2) + 
    xlab("Day of the Week") + ylab("Total Motor Vehicle thefts")

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha=0.3) + 
    xlab("Day of the Week") + ylab("Total Motor Vehicle thefts")

### A heatmap

table(mvt$Weekday, mvt$Hour)

DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))

ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1))

# Let's have a different colour for each day of week
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1, color = Var1), size=2)

DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered = TRUE, 
                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# make heatmap
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
    scale_fill_gradient(name="Total MV thefts") + theme(axis.title.y = element_blank())

# change colour theme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
    scale_fill_gradient(name="Total MV thefts", low="white", high = "red") + 
    theme(axis.title.y = element_blank())


### Geographical hot spot map
library(maps)
library(ggmap)

chicago <- get_map(location = "chicago", zoom = 11)

ggmap(chicago)

ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude, y = Latitude))


LatLongCounts <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLongCounts)

LatLongCounts$Long <- as.numeric(as.character(LatLongCounts$Var1))
LatLongCounts$Lat <- as.numeric(as.character(LatLongCounts$Var2))

ggmap(chicago) + geom_point(data = LatLongCounts, aes(x = Long, y = Lat, color = Freq, size = Freq))

ggmap(chicago) + 
    geom_point(data = LatLongCounts, aes(x = Long, y = Lat, color = Freq, size = Freq)) + 
    scale_color_gradient(low="yellow", high="red")

ggmap(chicago) + 
    geom_tile(data = LatLongCounts, aes(x = Long, y = Lat, alpha = Freq), fill = "red")

#
# Quick Quiz
#

LatLongCounts2 <- subset(LatLongCounts, Freq != 0)

nrow(LatLongCounts) - nrow(LatLongCounts2)

ggmap(chicago) + 
    geom_tile(data = LatLongCounts2, aes(x = Long, y = Lat, alpha = Freq), fill = "red")

### Heatmap of the united states

murders <- read.csv("murders.csv")
str(murders)

statesMap <- map_data("state")
str(statesMap)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill="white", color="black")

murders$region <- tolower(murders$State)

murderMap <- merge(statesMap, murders, by = "region")
str(murderMap)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "black", high = "red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "black", high = "red", guide = "legend")

# create murder rate 

murderMap$MurderRate <- murderMap$Murders / murderMap$Population *100000

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "black", high = "red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

#
# Quick Question
#

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "black", high = "red", guide = "legend")