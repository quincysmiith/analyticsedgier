#############
#
# Analytics Edge
# Intro to Linear Regression Walkthrough
#
#############

# At Home
setwd("~/Documents/analyticsedgier/Week 02. Linear Regression/Intro to Linear Lecture")

wine <- read.csv("wine.csv")

str(wine)

summary(wine)

# Create linear regression model with one variable

model1 <- lm(Price ~ AGST, data = wine)
summary(model1)

model1$residuals
SSE = sum(model1$residuals^2)

model2 <- lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

SSE = sum(model2$residuals^2)


model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)

SSE = sum(model3$residuals^2)


#
# Quick Quiz
#

model4 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)


#
# Understanding the model
#

summary(model3)


model5 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model5)
#adjusted R-squared is higher in this model after removing FrancePop


#
# Correlation and colinearality
#

model6 <- lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model6)


#
# Quick quiz
#

cor(wine$HarvestRain,wine$WinterRain)

#
# Making Predictions
#

#read in test data
wineTest <- read.csv("wine_test.csv")
str(wineTest)

predictTest <- predict(model5, newdata=wineTest)
predictTest

SSE <- sum((wineTest$Price - predictTest)^2)
SSE
