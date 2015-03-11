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


# which foods contain more than 10,000mg of sodium
high_sodium <- subset(usda, Sodium > 10000)
nrow(high_sodium)

high_sodium$Description

# which index is caviar in the data
match("CAVIAR", usda$Description)

usda$Sodium[4154]

# OR... in one line
usda$Sodium[match("CAVIAR", usda$Description)]

summary(usda$Sodium)
sd(usda$Sodium, na.rm=TRUE)

# Creating simple plots
plot(usda$Protein, usda$TotalFat, xlab="Protein", ylab="Fat", main="Protein vs Fat", col="red")

hist(usda$VitaminC, xlab="Vitamin C (mg)", main="Histogram of Vitamin C levels",
     xlim = c(0,100), breaks = 2000)

boxplot(usda$Sugar, main="Sugar Levels", ylab="Sugar (g)")

#
# Adding variables
#

# Create a flag to indicate whether the sodium content in a food item is larger than the average

usda$Sodium[1] > mean(usda$Sodium, na.rm=TRUE)

usda$Sodium[50] > mean(usda$Sodium, na.rm=TRUE)

high_sodium <- usda$Sodium > mean(usda$Sodium, na.rm=TRUE)

high_sodium <- as.numeric(usda$Sodium > mean(usda$Sodium, na.rm=TRUE))

usda$high_sodium <- as.numeric(usda$Sodium > mean(usda$Sodium, na.rm=TRUE))
str(usda)

usda$high_protein <- as.numeric(usda$Protein > mean(usda$Protein, na.rm=TRUE))
usda$high_fat <- as.numeric(usda$TotalFat > mean(usda$TotalFat, na.rm=TRUE))
usda$high_carb <- as.numeric(usda$Carbohydrate > mean(usda$Carbohydrate, na.rm=TRUE))

str(usda)

