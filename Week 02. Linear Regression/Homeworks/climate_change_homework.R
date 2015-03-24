#############
#
# Analytics Edge
# climate change homework
#
#############

# At work
setwd("C:/Users/marquin.smith/Downloads")

#
# Problem 1
#

climate <- read.csv("climate_change.csv")
str(climate)

# create training and test sets
climate_train <- subset(climate, Year < 2007)
climate_test <- subset(climate, Year >= 2007)

# build model
model <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climate_train)
summary(model)


# remove coefficients with high colinearality
model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=climate_train)
summary(model2)

# use step function to create new model
model3 <- step(model)
summary(model3)

# Get R squared of model produced from step function applied on test data
temp_prediction <- predict(model3, climate_test)

SSE <- sum((temp_prediction - climate_test$Temp)^2)
SST <- sum((mean(climate_train$Temp) - climate_test$Temp)^2)

R2 <- 1 - (SSE / SST)
R2
