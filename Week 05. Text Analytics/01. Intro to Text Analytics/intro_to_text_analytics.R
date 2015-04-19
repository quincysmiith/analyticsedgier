#############
#
# Analytics Edge
# Intro to text analytics homework
#
#############

# at home
setwd("~/Documents/analyticsedgier/Week 05. Text Analytics/01. Intro to Text Analytics")

tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)

# identify negative tweets in the data
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

# install new packages
## install.packages("tm")
library(tm)

## install.packages("SnowballC")
library(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus[[1]]

# change to lower case
corpus <- tm_map(corpus, tolower)
corpus[[1]]

corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]

#remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# lets look at a few "stop words"
stopwords("english")[1:10]


# remove stop words from corpus
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

#
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]


# creates matrix where each line referes to a tweet
# the headers of each column is a word, and the value in the matrix is the number of times 
# that word appears in that tweet
frequencies <- DocumentTermMatrix(corpus)
frequencies

# take a closer look at a select few entries
inspect(frequencies[1000:1005, 505:515])

# this can be described as sparse because there are many zero entries

#
findFreqTerms(frequencies, lowfreq=20)

#
sparse <- removeSparseTerms(frequencies, 0.995)
sparse

# convert sparse into dataframe
tweetsSparse <- as.data.frame(as.matrix(sparse))

# R has troubles with names that start with a number
# lets make these easier to work with

colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

# add variable indicating if a tweet is negative or not
# remember when we reduced the number of words we reduced the number of columns not the rows
tweetsSparse$Negative <- tweets$Negative

# create training and test set
library(caTools)

set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)

#
# Quick Quiz
#

findFreqTerms(frequencies, lowfreq=100)

####

library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

predictCART <- predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)

accuracy <- (294 + 18) / nrow(testSparse)
accuracy

# calculate baseline accuracy of just predicting non negative
table(testSparse$Negative)
300 / 355

# how well do random forests do?
library(randomForest)
set.seed(123)

tweetRF <- randomForest(Negative ~ ., data=trainSparse)

predictRF <- predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)

accuracy <- (293 + 21) / nrow(testSparse)
accuracy


#
# Quick Question
#

tweetLog <- glm(Negative ~ ., data=trainSparse, family="binomial")

tweetLog_pred <- predict(tweetLog, newdata=testSparse)
table(testSparse$Negative, tweetLog_pred > 0.5)

accuracy <- (257 + 34) / nrow(testSparse)
accuracy