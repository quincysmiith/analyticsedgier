#############
#
# Analytics Edge
# Predicting Stock Returns homework
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 06. Clustering/Homeworks")

stocks <- read.csv("StocksCluster.csv")
str(stocks)

table(stocks$PositiveDec)

6324 / nrow(stocks)

# Pairwise correlations
cor(stocks)

#which month has highest mean
summary(stocks)

#
library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

# create logistic model on training set and compute accuracy on test set
stocksLog <- glm(PositiveDec ~ ., data = stocksTrain, family='binomial')
pred <- predict(stocksLog, newdata = stocksTrain, type='response')
table(stocksTrain$PositiveDec, pred >= 0.5)
accuracy <- (990 + 3640) / nrow(stocksTrain)
accuracy

pred2 <- predict(stocksLog, newdata = stocksTest, type='response')
table(stocksTest$PositiveDec, pred >= 0.5)
accuracy <- (417 + 1553) / nrow(stocksTest)
accuracy

table(stocksTest$PositiveDec)

1897 / nrow(stocksTest)

# Now, let's cluster the stocks. The first step in this process is to remove the dependent variable
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

#
library(caret)

preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

# K means
k = 3
set.seed(144)

normTrainMatrix <- as.matrix(normTrain)
km <- kmeans(normTrainMatrix, centers = k)

table(km$cluster)


# Recall from the recitation that we can use the flexclust package to obtain training set and 
# testing set cluster assignments for our observations (note that the call to as.kcca may 
# take a while to complete):

library(flexclust)

km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata=normTest)

table(clusterTest)

stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

summary(stocksTrain1$PositiveDec)
summary(stocksTrain2$PositiveDec)
summary(stocksTrain3$PositiveDec)

stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)

stocksModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family='binomial')
stocksModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family='binomial')
stocksModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family='binomial')

summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel3)


predictTest1 <- predict(stocksModel1, newdata = stocksTest1, type = 'response')
table(stocksTest1$PositiveDec, predictTest1 > 0.5)
accuracy <- (30 + 774) / nrow(stocksTest1)
accuracy

predictTest2 <- predict(stocksModel2, newdata = stocksTest2, type = 'response')
table(stocksTest2$PositiveDec, predictTest2 > 0.5)
accuracy <- (388 + 757) / nrow(stocksTest2)
accuracy

predictTest3 <- predict(stocksModel3, newdata = stocksTest3, type = 'response')
table(stocksTest3$PositiveDec, predictTest3 > 0.5)
accuracy <- (49 + 13) / nrow(stocksTest3)
accuracy


# put all clusters together

AllPredictions <- c(predictTest1, predictTest2, predictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)

accuracy <- (467 + 1544) / (467 + 1110 + 353 + 1544)
accuracy
