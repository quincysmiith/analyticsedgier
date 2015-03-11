#############
#
# Analytics Edge
# Homework 1
# Chicago Crime
#
#############

setwd("C:/Users/marquin.smith/Downloads")

data <- read.csv("mvtWeek1.csv")

#
# PART 1
#

# How many rows in the data
nrow(data)

# How many columns in the data
ncol(data)

# What is the max of ID variable
max(data$ID)

# What is the minimum of "Beat" Variable
min(data$Beat)

# how many observations have TRUE in Arrest
sum(as.numeric(data$Arrest))

# how many obs have location description of ALLEY
nrow(subset(data, LocationDescription == "ALLEY"))


#
# PART 2
#


# what format is the date in
data$Date[130950]


# month and date of median date
DateConvert <- as.Date(strptime(data$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

# which month did the fewest motor vehicle thefts occur

data$Month <- months(DateConvert)
data$Weekday <- weekdays(DateConvert)

data$Date <- DateConvert

table(data$Month)

# which weekday did the most motor thefts occur
table(data$Weekday)

# which month had most motor vehicle thefts where an arrest was made
table(data$Month[data$Arrest == TRUE])


#
# PART 3
#


hist(data$Date, breaks=100)


boxplot(data$Date, data$Arrests, na.rm=TRUE)


# what proportion of motor vehicle thefts ended in an arrest in 2001
table(data$Arrest[data$Year == 2001])

2152 / (2152 + 18517)


# what proportion of motor vehicle thefts ended in an arrest in 2007

table(data$Arrest[data$Year == 2007])

1212 / (13068 + 1212)


# what proportion of motor vehicle thefts ended in an arrest in 2012
table(data$Arrest[data$Year == 2012])

550 / (550 + 13542)


# what are the top 5 locations for mnotor vehicle thefts
sort(table(data$LocationDescription))



top_5_locations <- subset(data, LocationDescription == "STREET"|
                              LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|
                              LocationDescription == "ALLEY"|
                              LocationDescription == "GAS STATION"|
                              LocationDescription == "DRIVEWAY - RESIDENTIAL")

nrow(top_5_locations)


top_5_locations$LocationDescription = factor(top_5_locations$LocationDescription)

# which area of the top 5 has the highest arrest rate
table(top_5_locations$LocationDescription, top_5_locations$Arrest)

PercentArrests <- function(non_arrests, arrests){
    arrests / (non_arrests + arrests) 
}

PercentArrests(2059,249) # ALLEY
PercentArrests(1543,132) # DRIVEWAY - RESIDENTIAL
PercentArrests(1672,439) # GAS STATION
PercentArrests(13249,1603) # PARKING LOT/GARAGE(NON.RESID.)
PercentArrests(144969,11595) # STREET


# which day of the week do most of the thefts occur at gas stations
table(top_5_locations$Weekday[top_5_locations$LocationDescription == "GAS STATION"])

# on which day of the week do the least thefts occur from resiential driveways
table(top_5_locations$Weekday[top_5_locations$LocationDescription == "DRIVEWAY - RESIDENTIAL"])