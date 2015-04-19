#############
#
# Analytics Edge
# Predicting Parole Homework
#
#############

# At work
#setwd("C:/Users/marquin.smith/Downloads")

#At home
setwd("~/Documents/analyticsedgier/Week 03. Logistic Regression/Homeworks")

parole <- read.csv("parole.csv")

str(parole)

table(parole$violator)

table(parole$state)
table(parole$race)
table(parole$crime)


parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)


#
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio=0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole,split == FALSE)

# Building the logistic model

mod1 <- glm(violator ~ ., data=train, family="binomial")
summary(mod1)


# intro to logistic notes will help
logOdds <- -4.2411574 + (0.3869904 * 1) + (0.8867192 *1) + (-0.0001756 * 50) +
    (-0.1238867 * 3) + (0.0802954 * 12) + (0.6837143)
    
odds <- exp(logOdds)

prob <- 1 / ( 1 + exp(-logOdds))

#predict on test set
parolePred <- predict(mod1, type="response", newdata=test)
max(parolePred)


table(test$violator, parolePred > 0.5)
sensitivity <- 12 / (11+12)
specificity <- 167 / (167+12)

accuracy <- (167+12) / (167+12+11+12)

sensitivity
specificity
accuracy

library("ROCR")

ROCRpredTest <- prediction(parolePred , test$violator)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc