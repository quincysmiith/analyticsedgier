#############
#
# Analytics Edge
# Predicting Loan Repayments Homework
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 03. Logistic Regression/Homeworks")

#
loans <- read.csv("loans.csv")

str(loans)
summary(loans)

table(loans$not.fully.paid)
1533 / 9578

#
library("mice")
set.seed(144)
vars_for_imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars_for_imputation]))
loans[vars_for_imputation] = imputed

loans_imputed <- read.csv("loans_imputed.csv")

# create logistic regression model
library("caTools")
set.seed(144)

split <- sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)

loansTrain <- subset(loans_imputed, split == TRUE)
loansTest <- subset(loans_imputed, split == FALSE)

mod1 <- glm(not.fully.paid ~ ., data=loansTrain, family="binomial" )
summary(mod1)


#
fico700 <- -0.009317 * 700
fico710 <- -0.009317 * 710

fico700 - fico710
exp(fico700 - fico710)


#
loansPred <- predict(mod1, newdata=loansTest, type="response")
loansTest$predicted.test <- loansPred

table(loansTest$not.fully.paid, loansPred > 0.5)

accuracy <- (2400 + 3) / (2400 + 13 + 457 + 3)
accuracy

baseline <- (2400 + 13) / (2400 + 13 + 457 + 3)
baseline

library("ROCR")

ROCRpredTest <- prediction(loansPred , loansTest$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# build logistic model with only interest rate
mod2 <- glm(not.fully.paid ~ int.rate, data=loansTrain, family="binomial")
summary(mod2)

# maximum probability of not paid using this model
loansPred2 <- predict(mod2, newdata=loansTest, type="response")
max(loansPred2)

table(loansTest$not.fully.paid, loansPred2 > 0.5)

ROCRpredTest <- prediction(loansPred2 , loansTest$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


10 * exp(0.06 * 3)


# Simple investment strategy
loansTest$profit = exp(loansTest$int.rate*3) - 1
loansTest$profit[loansTest$not.fully.paid == 1] = -1

max(loansTest$profit)


#
highInterest <- subset(loansTest, int.rate >= 0.15)
mean(highInterest$profit)

table(highInterest$not.fully.paid)
110 / 437


#
cutoff <- sort(highInterest$predicted.test, decreasing=FALSE)[100]
low_risk <- subset(highInterest, predicted.test <= cutoff)
sum(low_risk$profit)

table(low_risk$not.fully.paid)
