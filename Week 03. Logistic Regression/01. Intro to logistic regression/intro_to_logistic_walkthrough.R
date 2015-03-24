#############
#
# Analytics Edge
# Intro to Logistic Regression Walkthrough
#
#############

#
# Quick quiz
#

logit <- -1.5 + (3*1) + (-0.5*5)
logit

exp(-1)

1 / (1 + exp(1))


#
# Logistic Regression in R
#

# Build a logistic regression model to predict poor care

setwd("~/Documents/analyticsedgier/Week 03. Logistic Regression/01. Intro to logistic regression")

quality <- read.csv("quality.csv")

str(quality)

# How many people received poor and good care
table(quality$PoorCare)

# baseline of logistic model is just to predict most frequent outcome
98/131

#install.packages("caTools")

library("caTools")

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)

# in split TRUE indicates training set while false indicates test set

qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)

# lets build the model using the test set
# the "family=binomial" tells the general linear model to create a logistic model
qualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain,family=binomial)

summary(qualityLog)

predictTrain <- predict(qualityLog, type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

#
# Quick question
#

qualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(qualityLog2)

#
# Thresholding
#

table(qualityTrain$PoorCare, predictTrain > 0.5)

# Sensitivity
10/25

# Specificity
70/74

# increase threshold
table(qualityTrain$PoorCare, predictTrain > 0.7)

# Sensitivity
8/25

# Specificity
73/74


# decrease threshold
table(qualityTrain$PoorCare, predictTrain > 0.2)

# Sensitivity
16/25

# Specificity
54/74

#
# ROC curves
#

install.packages("ROCR")
library("ROCR")

# plotting ROC curves
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)

#add colours
plot(ROCRperf, colorize=TRUE)

# add increment labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))


#
# Quick Question
#

# Calculate AUC of prediction on test set
predictTest <- predict(qualityLog, type="response", newdata=qualityTest)

ROCRpredTest <- prediction(predictTest , qualityTest$PoorCare)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
