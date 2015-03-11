#############
#
# Analytics Edge
# Marquin Smith
# Understanding Food
#
#############

# AT HOME
setwd("~/Documents/analyticsedgier/Week 01. Introduction to Analytics")
getwd()


usda <- read.csv("USDA.csv")
str(usda)
summary(usda)


usda$Description[which.max(usda$Sodium)]

