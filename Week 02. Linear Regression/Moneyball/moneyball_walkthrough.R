#############
#
# Analytics Edge
# Moneyball: the power of analytics
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 02. Linear Regression/Moneyball")

baseball <- read.csv("baseball.csv")
str(baseball)

moneyball <- subset(baseball, Year < 2002)
str(moneyball)

#create run difference column
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)

plot(moneyball$RD, moneyball$W)

WinsReg <- lm(W ~ RD, data=moneyball)
summary(WinsReg)


#
# Quick question
#

713 - 614

80.881375 + (0.105766 * 99)


#
# Predicting runs
#

str(moneyball)

RunsReg <- lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)


RunsReg <- lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

#
# Quick Question
#

my_OBP <- 0.311
my_SLG <- 0.405

-804.63 + (2737.77 * my_OBP) + (1584.91 * my_SLG)


-837.38 + (2913.60 * 0.297) + (1514.29 * 0.370)


#Eric_Chavez
my_OBP <- 0.338
my_SLG <- 0.540

EC <- -804.63 + (2737.77 * my_OBP) + (1584.91 * my_SLG)

runs <- function(OBP, SLG){
    -804.63 + (2737.77 * OBP) + (1584.91 * SLG)
}

eric <- runs(0.338, 0.540)
jeremy <- runs(0.391, 0.450)
frank <- runs(0.369, 0.374)
greg <- runs(0.313, 0.447)
carlos <- runs(0.361, 0.5)


#
# quick question
#

teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012 <- c(94,88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 <- c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)
