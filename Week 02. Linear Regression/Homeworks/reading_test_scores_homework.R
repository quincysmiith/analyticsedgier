#############
#
# Analytics Edge
# reading test scores homework
#
#############

# At work
setwd("C:/Users/marquin.smith/Downloads")


# load data
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

#how many students in data
nrow(pisaTrain)

# average test scores for male and female
tapply(pisaTrain$readingScore, pisaTrain$male, mean, na.rm=TRUE)

#which columns have na's
summary(pisaTrain)

# remove rows with NA 
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)

nrow(pisaTrain)
nrow(pisaTest)


#
LinReg <- lm(readingScore ~ ., data=pisaTrain)
summary(LinReg)

SSE <- sum(LinReg$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE

29.542707 * 2


# difference between max and min test scores in predicted

pisaPredict <- predict(LinReg, newdata=pisaTest)
range(pisaPredict)
max(pisaPredict) - min(pisaPredict)


# what is the SSE and RMSE for the model on the test set
SSE <- sum((pisaPredict - pisaTest$readingScore)^2)
SSE

RMSE <- sqrt(SSE/nrow(pisaTest))
RMSE


# 

mean(pisaTrain$readingScore)

SST <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST

R2 <- 1 - (SSE / SST)
R2
