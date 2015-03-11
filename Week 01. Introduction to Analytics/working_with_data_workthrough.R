##############
#
# Analytics Edge
# An intro to R
# 
##############

#
# Loading Data Files
#

# For work

setwd("C:/Users/marquin.smith/Downloads")
getwd()

who <- read.csv("WHO.csv")

#lets see what kind of data we have just loaded
str(who)

summary(who)

#Get the data for countries in Europe
who_europe <- subset(who, Region == "Europe")

str(who_europe)

#lets save this subsetted data to csv
write.csv(who_europe, "who_europe.csv")


# lets remove "who_europe" from the environment
# we may want to do this in situations where an object is taking up a lot of space
ls()
remove("who_europe")
ls()



#
# Data analysis - summary statistics and scatterplots
#


who$Under15
mean(who$Under15)
sd(who$Under15)

summary(who$Under15)

# this tells us there is a country with 13% of its population less than 15
# lets find out which country this is
which.min(who$Under15)

# this returns the row number with the minimum.

who$Country[86]
#or in a one liner
who$Country[which.min(who$Under15)]


# lets do the same for the maximum
which.max(who$Under15)

who$Country[124]
# ... or

who$Country[which.max(who$Under15)]


# lets plot GNI vs Fertility rate
plot(who$GNI, who$FertilityRate)


# There are some outliers that we should look into more closely
outliers <- subset(who, GNI > 10000 & FertilityRate > 2.5)

# how many rows are then in outliers?
nrow(outliers)

outliers[c("Country", "GNI", "FertilityRate")]


# QUICK QUESTION
mean(who$Over60)

who$Country[which.min(who$Over60)]

who$Country[which.max(who$LiteracyRate)]




#
# Data Analysis - Plots and summary tables
#


# plot a histogram of the proportion of cellular subscribers
hist(who$CellularSubscribers)

# lets make a box plot of life expectancy sorted by region
boxplot(who$LifeExpectancy ~ who$Region)

boxplot(who$LifeExpectancy ~ who$Region, xlab="", ylab="Life Expactancy", main="Life expectancy of countries by region")

table(who$Region)

# what is the mean of Over60 by region
tapply(who$Over60, who$Region, mean)

tapply(who$LiteracyRate, who$Region, min)

tapply(who$LiteracyRate, who$Region, min, na.rm=TRUE)


# QUICK QUESTION
tapply(who$ChildMortality, who$Region, mean)
