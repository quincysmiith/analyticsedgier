#############
#
# Analytics Edge
# Voting behaviour homework
#
#############

# at home
setwd("~/Documents/analyticsedgier/Week 04. Trees/Homeworks")

gerber <- read.csv("gerber.csv")
str(gerber)

# how many people voted
nrow(subset(gerber, voting==1)) / nrow(gerber)

# proportion voted in each group

# civic duty
gerber_civic <- gerber[gerber$civicduty == 1,]
nrow(subset(gerber[gerber$civicduty == 1,], voting==1)) / nrow(gerber[gerber$civicduty == 1,])

#hawthorn
nrow(subset(gerber[gerber$hawthorne == 1,], voting==1)) / nrow(gerber[gerber$hawthorne == 1,])

#self
nrow(subset(gerber[gerber$self == 1,], voting==1)) / nrow(gerber[gerber$self == 1,])

#neighbours
nrow(subset(gerber[gerber$neighbors == 1,], voting==1)) / nrow(gerber[gerber$neighbors == 1,])


##
gerberLog <- glm(voting ~ hawthorne + civicduty + neighbors + self,
                 data = gerber, family="binomial")
summary(gerberLog)

##
predictLog <- predict(gerberLog, type="response")

table(gerber$voting, predictLog > 0.3)
accuracy <- (134513 + 51966) / nrow(gerber)
accuracy

##
table(gerber$voting, predictLog > 0.5)
accuracy <- (235388) / nrow(gerber)
accuracy

##
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

##

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

##
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

gerberControl <- subset(gerber, control == 1)
table(gerberControl$sex, gerberControl$voting)

male <- 29015 / (66809 + 29015)
female <- 27715 / (67704 + 27715)
male
female

gerberCivic <- subset(gerber, civicduty == 1)
table(gerberCivic$sex, gerberCivic$voting)

male <- 6165 / (12937 + 6165)
female <- 5856 / (13260 + 5856)
male
female

##
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6)

0.34 - 0.296638

##
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6)

female <- abs(0.290456 -0.334176)
male <- abs(0.302795 - 0.345818)
female
male
female - male

##
gerberLog2 <- glm(voting ~ control + sex, data=gerber, family="binomial")
summary(gerberLog2)

##
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerberLog2, newdata=Possibilities, type="response")

abs(0.290456 - 0.2908065)

##
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

##
predict(LogModel2, newdata=Possibilities, type="response")

abs(0.290456 - 0.2904558)
