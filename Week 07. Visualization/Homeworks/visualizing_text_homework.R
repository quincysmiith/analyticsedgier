#############
#
# Analytics Edge
# Visualizing Text Homework
# 
#############

# At work
setwd("C:/Users/marquin.smith/Downloads")

tweets <- read.csv("tweets.csv")

library(tm)

# 1) Create a corpus using the Tweet variable
corpus <- Corpus(VectorSource(tweets$Tweet))

# 2) Convert the corpus to lowercase (don't forget to type "corpus = tm_map(corpus, PlainTextDocument)" in your R console right after this step)
corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, PlainTextDocument)

# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)

# 4) Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))

# 5) Build a document-term matrix out of the corpus
frequencies <- DocumentTermMatrix(corpus)

# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(frequencies))


install.packages("wordcloud")
library(wordcloud)

?wordcloud

Words <- colnames(allTweets)
str(Words)

Freq <- colSums(allTweets)
str(Freq)

wordcloud(Words, Freq, scale=c(2, 0.25))



### this time remove apple
# 1) Create a corpus using the Tweet variable
corpus <- Corpus(VectorSource(tweets$Tweet))

# 2) Convert the corpus to lowercase (don't forget to type "corpus = tm_map(corpus, PlainTextDocument)" in your R console right after this step)
corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, PlainTextDocument)

# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)

# 4) Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))

# 5) Build a document-term matrix out of the corpus
frequencies <- DocumentTermMatrix(corpus)

# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(frequencies))

# create word cloud with this updated version
Words <- colnames(allTweets)
Freq <- colSums(allTweets)
wordcloud(Words, Freq, scale=c(2, 0.25))

wordcloud(Words, Freq, scale=c(2, 0.25), random.order=FALSE)


?brewer.pal

display.brewer.pal(3, "Accent")
display.brewer.pal(3, "Set2")
display.brewer.pal(3, "YlOrRd")

display.brewer.pal(3, "Dark2")
display.brewer.pal(6, "Paired")
display.brewer.pal(7, "Greys")
display.brewer.pal(7, "Blues")

wordcloud(Words, Freq, scale=c(2, 0.25), colors=brewer.pal(9, "Blues"))

wordcloud(Words, Freq, scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
wordcloud(Words, Freq, scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
