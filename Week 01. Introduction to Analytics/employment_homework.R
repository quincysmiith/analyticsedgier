#############
#
# Analytics Edge
# Homework 1
# Employment data
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 01. Introduction to Analytics")

cps <- read.csv('CPSData.csv')


# How many interviewees are in the data
nrow(cps)

# what is the most common industry
names(cps)
sort(table(cps$Industry))

# which state has the fewest interviewees
sort(table(cps$State))

# what proportion of interviewees are citizens
sort(table(cps$Citizenship))

(131302 - 7590) / 131302

# which races have more than 250 respondents that are Hispanic
unique(cps$Hispanic)
cps_hispanic <- subset(cps, Hispanic == 1)

sort(table(cps_hispanic$Race))

# Which columns have na's
summary(cps)

# how are being married and various demographics related
table(cps$Region, is.na(cps$Married))

table(cps$Age, is.na(cps$Married))

table(cps$Sex, is.na(cps$Married))

table(cps$Citizenship, is.na(cps$Married))


# how many states had all interviewees living in a non metro area
table(cps$State, is.na(cps$MetroAreaCode))

# which region had largest proportion of metro
table(cps$Region, is.na(cps$MetroAreaCode))

20010 / (20010+10674)
20330 / (20330+5609)
31631 / (31631+9871)
25093 / (25093+8084)

#
cps$MetroBool <- as.logical(cps$MetroAreaCode)
tapply(is.na(cps$MetroBool), cps$State, mean)

# 
mam <- read.csv("MetroAreaCodes.csv")
cm <- read.csv("CountryCodes.csv")


#
cps2 <- merge(cps, mam, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

names(cps)
names(cps2)

table(is.na(cps2$MetroArea))

# Which metro area has largest number of interviewees
sort(table(cps2$MetroArea))

#Which state has highest proportion of hispanic interviewees
sort(tapply(cps2$Hispanic, cps2$MetroArea, mean, na.rm=TRUE))

# How many metro areas have at 20% Asian interviewees
sort(tapply(cps2$Race == "Asian", cps2$MetroArea, mean))

#
sort(tapply(cps2$Education == "No high school diploma", cps2$MetroArea, mean, na.rm=TRUE))

# merge data with country of birth
names(cm)
str(cps2)

cps3 <- merge(cps2, cm, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
names(cps2)
names(cps3)

# how many of the new column Country are blank
table(is.na(cps3$Country))

# Outside north america what is the most common place of birth
sort(table(cps3$Country))

# What proportion of new jersey were born outside the US
table(is.na(cps3$Country[cps3$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"]))

nj_mask <- cps3$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"

sort(table(cps3$Country[nj_mask]))

(((3736 / 5404) - 1) * -1)

# Which country has highest volume of india born
india_mask <- cps3$Country == "India"

sort(table(cps3$MetroArea[india_mask]))


# Which country has highest volume of Brazil born
brazil_mask <- cps3$Country == "Brazil"

sort(table(cps3$MetroArea[brazil_mask]))

# Which country has highest volume of Somalia born
somalia_mask <- cps3$Country == "Somalia"

sort(table(cps3$MetroArea[somalia_mask]))