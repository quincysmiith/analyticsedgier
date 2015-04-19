#############
#
# Analytics Edge
# Census homework
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 04. Trees/Homeworks")

census <- read.csv("census.csv")
str(census)

library(caTools)

set.seed(2000)
split <- sample.split(census$over50k, SplitRatio=0.6)
censusTrain <- subset(census, split==TRUE)
censusTest <- subset(census, split==FALSE)

censusLog <- glm(over50k ~ ., data=censusTrain, family=binomial)
summary(censusLog)

censusLog_pred <- predict(censusLog, newdata=censusTest, type="response")
table(censusTest$over50k, censusLog_pred > 0.5)

accuracy <- (9051 + 1888) / nrow(censusTest)
accuracy

table(censusTest$over50k)
9713 / nrow(censusTest)

library("ROCR")
ROCRpredTest <- prediction(censusLog_pred , censusTest$over50k)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

##
censusTree <- rpart(over50k ~ ., data=censusTrain, method="class")
prp(censusTree)

censusTree_pred <- predict(censusTree, newdata=censusTest, type="class")
table(censusTest$over50k, censusTree_pred)

accuracy <- (9243 + 1596) / nrow(censusTest)
accuracy

##
censusTree_pred2 <- predict(censusTree, newdata=censusTest)
censusTree_pred2[,2]

ROCRpred <- prediction(censusTree_pred2[,2], censusTrain$over50k)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)

##
set.seed(1)
censusTrainSmall <-  censusTrain[sample(nrow(censusTrain), 2000), ]

library("randomForest")
set.seed(1)
censusForest <- randomForest(over50k ~ ., data=censusTrainSmall)

censusForest_pred <- predict(censusForest, newdata=censusTest)
table(censusTest$over50k, censusForest_pred)
accuracy <- (9586 + 1093) / nrow(censusTest)
accuracy

##
vu <- varUsed(censusForest, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

##
varImpPlot(censusForest)

##
numFolds <- trainControl(method = "cv", number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

set.seed(2)
train(over50k ~ ., data=censusTrain, method = "rpart", trControl=numFolds, tuneGrid=cartGrid)

##
censusTreeCV <- rpart(over50k ~ ., data=censusTrain, method="class", cp=0.002)
prp(censusTreeCV)

censusTreeCV_pred <- predict(censusTreeCV, newdata=censusTest, type="class")
table(censusTest$over50k, censusTreeCV_pred)

accuracy <- (9178 + 1838) / nrow(censusTest)
accuracy