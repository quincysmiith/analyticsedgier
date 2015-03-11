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
