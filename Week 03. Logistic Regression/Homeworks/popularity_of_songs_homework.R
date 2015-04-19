#############
#
# Analytics Edge
# Popularity of Songs Homework
#
#############

# At home
setwd("~/Documents/analyticsedgier/Week 03. Logistic Regression/Homeworks")

#load data
songs <- read.csv("songs.csv")
str(songs)


#how many obs in 2010?
table(songs$year)

table(songs$artistname)

mj <- subset(songs, artistname == "Michael Jackson" & Top10 == 1)
mj$songtitle

table(songs$timesignature)

# which song has highest tempo
songs$songtitle[which.max(songs$tempo)]

# split into train and test set
songTrain <- subset(songs, year < 2009.5)
songTest <- subset(songs, year > 2009.5)

nrow(songTrain)

nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")

songTrain <- songTrain[ , !(names(songTrain) %in% nonvars) ]
songTest <- songTest[ , !(names(songTest) %in% nonvars) ]

mod1 <- glm(Top10 ~ ., data = songTrain, family="binomial")
summary(mod1)

# correlation between loudness and energy
cor(songTrain$loudness, songTrain$energy)


#
mod2 <- glm(Top10 ~ . - loudness, data=songTrain, family=binomial)
summary(mod2)

#
mod3 <- glm(Top10 ~ . - energy, data=songTrain, family=binomial)
summary(mod3)

#
songTest2 <- songTest[ , !(names(songTrain) %in% c("energy")) ]
songPred <- predict(mod3, newdata=songTest, type="response")

table(songTest$Top10, songPred > 0.45)
accuracy <- (309 + 19) / (309 + 5 + 40 + 19)
accuracy

(309 + 5) / (309 + 5 + 40 + 19)

sensitivity <- 19 / (40 + 19)
specificity <- 309 / (309 + 5)

sensitivity
specificity