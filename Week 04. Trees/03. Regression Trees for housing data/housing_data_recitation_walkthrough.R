#############
#
# Analytics Edge
# Regression trees for housing walkthrough
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 04. Trees/03. Regression Trees for housing data")

boston <- read.csv("boston.csv")
str(boston)
plot(boston$LON, boston$LAT)
# Add pints that lie on boston river

points(boston$LON[boston$CHAS ==1], boston$LAT[boston$CHAS ==1], col="blue", pch=19)
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col="red", pch=19)

summary(boston$NOX)

points(boston$LON[boston$NOX > 0.55], boston$LAT[boston$NOX > 0.55], col="green", pch=19)

# create a new plot to investigate prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)

points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)#


plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

latlonlm <- lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)

# where does our linear regression model think the above average in price houses are

points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2],
       col="blue", pch="$")

library(rpart)
library(rpart.plot)

latlontree <- rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)

fittedvalues <- predict(latlontree)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2],
       col="blue", pch="$")

# we have suspicions that this tree is overfitting because of the complexity of the tree
# lets try and make the tree simpler

latlontree2 <- rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree2)
text(latlontree2)

# lets try and make some sense of this on the map
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2], col="red", pch=19)


library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio=0.7)
bostonTrain <- subset(boston, split==TRUE)
bostonTest <- subset(boston, split==FALSE)

# create linear regression model
linreg <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
             data=bostonTrain)

summary(linreg)

# lets calculate SSE so we can compare lin regresssion to CART method

linreg.pred <- predict(linreg, newdata=bostonTest)
linreg.sse <- sum((linreg.pred - bostonTest$MEDV)^2)
linreg.sse

tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
             data=bostonTrain)

prp(tree)

tree.pred <- predict(tree, newdata=bostonTest)
tree.sse <- sum((tree.pred - bostonTest$MEDV)^2)
tree.sse

# Applying Cross validation
library(caret)
library(e1071)

tr.control <- trainControl(method="cv", number = 10)
cp.grid <- expand.grid(.cp=(0:10) * 0.001)
cp.grid

tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, 
            data = bostonTrain, method="rpart", trControl=tr.control, tuneGrid=cp.grid)

tr

best.tree <- tr$finalModel
prp(best.tree)
