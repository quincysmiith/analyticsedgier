#############
#
# Analytics Edge
# moneyball in NBA
#
#############

# At work
setwd("C:/Users/marquin.smith/Downloads")

NBA <- read.csv("NBA_train.csv")
str(NBA)

# R puts an "X" in front of a variable if it starts with a number

table(NBA$W, NBA$Playoffs)

# if a team wins 42 or more games they have a good chance of making the playoffs

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)

WinsReg <- lm(W ~ PTSdiff, data=NBA)
summary(WinsReg)


# what is the points diff to win 42 games

# Wins = 41 + 0.0326 * PTSdiff

(42-41) / 0.0326


PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=NBA)
summary(PointsReg)

PointsReg$residuals

SSE = sum(PointsReg$residuals^2)
SSE

RMSE = sqrt(SSE/nrow(NBA))
RMSE

mean(NBA$PTS)

summary(PointsReg)

# we would want to remove TOV first as it has the highest P-value
PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
summary(PointsReg2)

# we would want to remove DRB second as it this now has the highest P-value
PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
summary(PointsReg3)

# we would want to remove BLK thirdly as it this now has the highest P-value
PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(PointsReg4)

SSE_4 = sum(PointsReg4$residuals^2)
SSE_4

RMSE_4 = sqrt(SSE_4/nrow(NBA))
RMSE_4


# Make some predictions for 2012/2013 season
# load in test data
NBA_test <- read.csv("NBA_test.csv")

PointsPrediction <- predict(PointsReg4, newdata=NBA_test)

SSE = sum((PointsPrediction - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 <- 1 - (SSE / SST)
R2

RMSE <- sqrt(SSE / nrow(NBA_test))
RMSE