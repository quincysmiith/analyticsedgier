#############
#
# Analytics Edge
# Election Forecast Homework
#
#############

library(ggplot2)
library(maps)
library(ggmap)

# At home
setwd("~/Documents/analyticsedgier/Week 07. Visualization/Homeworks")

statesMap <- map_data("state")
str(statesMap)

table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")


polling <- read.csv("PollingImputed.csv")
str(polling)

test <- subset(polling, Year == 2012)
train <- subset(polling, Year == 2004 | Year == 2008)

mod2 <- glm(Republican~SurveyUSA+DiffCount, data=train, family="binomial")
TestPrediction <- predict(mod2, newdata=test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)
str(TestPredictionBinary)

# put in data frame
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, test$State)

head(predictionDataFrame)

table(predictionDataFrame$TestPredictionBinary)

summary(predictionDataFrame)

# this data frame needs to be merged woth maps data frame to allow plotting
predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

# make sure in the right order
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
    geom_polygon(color = "black")


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

str(predictionMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

subset(predictionDataFrame, region == "florida")


?geom_polygon


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction, size=3)) +
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")