#############
#
# Analytics Edge
# The D2Hawkeye story walkthrough
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 04. Trees/02. The D2Hawkeye Story")

claims <- read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009) / nrow(claims)

# split data into training and test set
library(caTools)
set.seed(88)
split <- sample.split(claims$bucket2009, SplitRatio = 0.6)

claimsTrain <- subset(claims, split == TRUE)
claimsTest <- subset(claims, split == FALSE)

#
# Quick question
#

#average age in training set
mean(claimsTrain$age)

table(claimsTrain$diabetes)
nrow(subset(claimsTrain, diabetes == 1)) / nrow(claimsTrain)

################

# Smart baseline
# 2009 bucket is the same as 2008 bucket

table(claimsTest$bucket2009, claimsTest$bucket2008)

accuracy <- (110138 + 10721 + 2774 + 1539 + 104) / nrow(claimsTest)
accuracy

# create penalty matrix
penaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),
                        byrow = TRUE, nrow = 5)
penaltyMatrix

as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix
# The penalty error for this smart bassline then becomes:
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix) /nrow(claimsTest)

# our goal now is to create a CART model that has an accuracy greater than 0.68 
# and a penalty error less than 0.7386055

#
# Quick Question
#

# if baseline is to predict most common

table(claimsTest$bucket2008)

table(claimsTest$bucket2009, claimsTest$bucket2009 == 1)

accuracy <- 122978 / nrow(claimsTest)
accuracy


# penalty error
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2009 == 1))[,1] * c(0,2,4,6,8)) / nrow(claimsTest)

##########

library(rpart)
library(rpart.plot)

claimsTree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + 
                        diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + 
                        bucket2008 + reimbursement2008,
                    data = claimsTrain, method="class", cp=0.00005)

prp(claimsTree)

predictTest <- predict(claimsTree, newdata = claimsTest, type="class")
table(claimsTest$bucket2009, predictTest)
accuracy <- (114141 + 16102 + 118 + 201 + 0) / nrow(claimsTest)
accuracy

#penalty error
sum(as.matrix(table(claimsTest$bucket2009, predictTest)) * penaltyMatrix) / nrow(claimsTest)

# this penalty is bigger than in our baseline
# this is because the lower number appear much more frequently and hence we predict them more often
# how can we fix this
# the rpart function allows you to specify a penalty matrix as one of its functions

claimsTree2 <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + 
                        diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + 
                        bucket2008 + reimbursement2008,
                    data = claimsTrain, method="class", cp=0.00005,
                    parms=list(loss=penaltyMatrix))

predictTest2 <- predict(claimsTree2, newdata = claimsTest, type="class")
table(claimsTest$bucket2009, predictTest2)

accuracy <- (94310 + 18942 + 4692 + 636 + 2) / nrow(claimsTest)
accuracy


sum(as.matrix(table(claimsTest$bucket2009, predictTest2)) * penaltyMatrix) / nrow(claimsTest)
