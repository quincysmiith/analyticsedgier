#############
#
# Analytics Edge
# Letter Recognition homework
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 04. Trees/Homeworks")

letters <- read.csv("letters_ABPR.csv")

#
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)

split <- sample.split(letters$isB, SplitRatio=0.50)
lettersTrain <- subset(letters, split==TRUE)
lettersTest <- subset(letters, split==FALSE)

table(letters$isB)
2350 / nrow(letters)

##

CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")

CARTb_pred <- predict(CARTb, newdata = lettersTest, type="class")
table(lettersTest$letter, CARTb_pred)

accuracy <- (380 + 340 + 386 + 352 ) / nrow(lettersTest)
accuracy

##
library("randomForest")
set.seed(1000)
forestB <- randomForest(isB ~ . -letter, data=lettersTrain)
#summary(forestB)

forestB_pred <- predict(forestB, newdata=lettersTest)

table(lettersTest$isB, forestB_pred)
accuracy <- (1165 + 374) / nrow(lettersTest)
accuracy

###
letters$letter = as.factor( letters$letter ) 

set.seed(2000)
split2 <- sample.split(letters$letter, SplitRatio = 0.5)
lettersTrain2 <- subset(letters, split2 == TRUE)
lettersTest2 <- subset(letters, split2 == FALSE)

table(lettersTest2$letter)


401/ nrow(lettersTest2)

##

letterTree <- rpart(letter ~ . - isB, data=lettersTrain2, method="class")
letterTree_pred <- predict(letterTree, newdata = lettersTest2, type="class")
table(lettersTest2$letter, letterTree_pred)

(348 + 318 + 363 + 340) / nrow(lettersTest2)

##

letterForest <- randomForest(letter ~ . - isB, data=lettersTrain2)
letterForest_pred <- predict(letterForest, newdata = lettersTest2, type="class")
table(lettersTest2$letter, letterForest_pred)

(390 + 380 + 393 + 369) / nrow(lettersTest2)
