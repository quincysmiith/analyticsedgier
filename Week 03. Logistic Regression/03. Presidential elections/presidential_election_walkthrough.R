#############
#
# Analytics Edge
# Presidential Election Walkthrough
#
#############

#At home
setwd("~/Documents/analyticsedgier/Week 03. Logistic Regression/03. Presidential elections")

polling <- read.csv("PollingData.csv")
str(polling)

table(polling$Year)

summary(polling)

# Lets install the mice package which will be used to impute values

#install.packages("mice")

library("mice")

simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
str(simple)
summary(simple)

set.seed(144)

#Impute figures for Rasmussen and SurveyUSA
imputed <- complete(mice(simple))
summary(imputed)

# put figures back in main table
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)



#
# A sophisticated baseline method
#

train <- subset(polling, Year == 2004 | Year == 2008)
test <- subset(polling, Year == 2012)

table(train$Republican)

# sign is a function that reurns 1, 0 or -1 depending on the sign of the input

sign(20)
sign(-10)
sign(0)

table(sign(train$Rasmussen))

table(train$Republican, sign(train$Rasmussen))

#
# Logistic regression models
#

# We should consider colinearity because they are all measuring the same thing
cor(train[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")])

#PropR has the highest correlation with Republican so we can use this as independent variable
mod1 <- glm(Republican ~ PropR, data=train, family="binomial")
summary(mod1)
summary(mod1)

pred1 <- predict(mod1, type="response")
table(train$Republican, pred1 >= 0.5)
# this gets 4 wrong - similar to smart baseline

# in the two variable model we should try 2 variables that are not correlated much with each other
# SurveyUSA and DiffCount

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data=train, family="binomial")
pred2 <- predict(mod2, type="response")
table(train$Republican, pred2 >= 0.5)
summary(mod2)


#
# Test set predictions
#

# Baseline of test set
table(test$Republican, sign(test$Rasmussen))

testPrediction <- predict(mod2, type="response", newdata= test)
table(test$Republican, testPrediction > 0.5)

# which state did we predict incorrectly

subset(test, testPrediction > 0.5 & Republican == 0)
