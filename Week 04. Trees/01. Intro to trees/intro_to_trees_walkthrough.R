#############
#
# Analytics Edge
# Intro to Trees walkthrough
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 04. Trees/01. Intro to trees")

stevens <- read.csv("stevens.csv")
str(stevens)
summary(stevens)

library(caTools)
set.seed(3000)

spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)

train <- subset(stevens, spl == TRUE)
test <- subset(stevens, spl == FALSE)

# Install packages needed to make decision trees
## install.packages("rpart") ##
library("rpart")

## install.packages("rpart.plot") ##
library("rpart.plot")


stevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                     data=train, method="class", minbucket=25)

#plot model
prp(stevensTree)

predictCart <- predict(stevensTree, newdata=test, type="class")

table(test$Reverse, predictCart)

accuracy <- (41 + 71) / (41 + 36 + 22 + 71)
accuracy

library(ROCR)
predictROC <- predict(stevensTree, newdata=test)
predictROC

pred <- prediction(predictROC[,2], test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

auc <- as.numeric(performance(pred, "auc")@y.values)
auc

stevensTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data=train, method="class", minbucket=5)
prp(stevensTree2)


stevensTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data=train, method="class", minbucket=100)
prp(stevensTree3)


#
# Random Forests
#

## install.packages("randomForest") ##
library("randomForest")

stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree = 200)

# When building random forests there is no method argument so we can't choose it to be "class"
# Instead we need to convert the dependent variable we are trying to predict into a factor variable
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)

stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree = 200)

predictForest <- predict(stevensForest, newdata = test)
table(test$Reverse, predictForest)

accuracy <- (42 + 75) / (42 + 35 + 18 + 75)
accuracy

#
# Quick quiz
#

set.seed(100)
stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree = 200)
table(test$Reverse, predictForest)

set.seed(200)
stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree = 200)
table(test$Reverse, predictForest)

#
# Cross validation
#

## install.packages("caret") ##
library("caret")

## install.packages("e1071") ##
library("e1071")

numFolds <- trainControl(method = "cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01, 0.5, 0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
      data=train, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)

stevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                       data=train, method="class", cp=0.19)

predictCV <- predict(stevensTreeCV, newdata=test, type="class")
table(test$Reverse, predictCV)

accuracy <- (59 + 64) / (59 + 18 + 29 + 64)
accuracy

prp(stevensTreeCV)
