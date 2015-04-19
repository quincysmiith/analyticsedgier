#############
#
# Analytics Edge
# Bringing test analytics to the courtroom - Recitation
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 05. Text Analytics/03. Bringing text analytics to the courtroom")

emails <- read.csv("energy_bids.csv", stringsAsFactors=FALSE)
str(emails)

emails$email[1]

# if the email was only represented on one line we could wrap this text with the following function
# strwrap(email$email[1])

# is this email responsive?
emails$responsive[1]

# no it isn't

#looking at the second email
emails$email[2]
 # is this responsive?
emails$responsive[2]


table(emails$responsive)

# most of the emails are unresponsive

#### pre processing

library(tm)

corpus <- Corpus(VectorSource(emails$email))

corpus[[1]]

#make lower case
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)

# remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# stem words
corpus <- tm_map(corpus, stemDocument)

corpus[[1]]


### Bag of words

#create document term matrix
# in other words frequency table of how often each word appears in each email
dtm <- DocumentTermMatrix(corpus)

dtm

#remove terms that dont appear in at least 3% of the documents
dtm <- removeSparseTerms(dtm, 0.97)

dtm

# make into data frame
labelledTerms <- as.data.frame(as.matrix(dtm))

# copy responsive column into dataframe
labelledTerms$responsive <- emails$responsive
str(labelledTerms)


### Building Models

library(caTools)

set.seed(144)

spl <- sample.split(labelledTerms$responsive, SplitRatio = 0.7)

#create train and test set
train <- subset(labelledTerms, spl==TRUE)
test <- subset(labelledTerms, spl==FALSE)

#building a cart model
library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ ., data=train, method="class")
prp(emailCART)


### Evaluating the model

pred <- predict(emailCART, newdata=test)
pred[1:10,]

pred.prod <- pred[,2]
table(test$responsive, pred.prod >= 0.5)

accuracy <- (195 + 25) / (195 + 20 + 17 + 25)
accuracy

#calculate baseline accuracy

table(test$responsive)
accuracy <- 215 / (215 + 42)
accuracy


### The ROC Curve

library(ROCR)

predROCR <- prediction(pred.prod, test$responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

#Calculate auc value
performance(predROCR, "auc")@y.values
