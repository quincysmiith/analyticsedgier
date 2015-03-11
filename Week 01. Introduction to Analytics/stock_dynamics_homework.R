#############
#
# Analytics Edge
# Homework 1
# Stock Dynamics
#
#############

# At work
#setwd("C:/Users/marquin.smith/Downloads")

#At home
setwd("~/Documents/analyticsedgier/Week 01. Introduction to Analytics")

###
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")


# change dates in data frames to actual dates

IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")


#
# Part 1
#

# Whats the minimum date in all of the datasets
min(IBM$Date)
min(GE$Date)
min(ProcterGamble$Date)
min(CocaCola$Date)
min(Boeing$Date)

# Whats the max year in the data sets
max(IBM$Date)
max(GE$Date)
max(ProcterGamble$Date)
max(CocaCola$Date)
max(Boeing$Date)

# what is the mean if IBM stock over this period
names(IBM)
mean(IBM$StockPrice)

# minimum of GE over this time period
min(GE$StockPrice)

# maximum of Coca cola over this time
max(CocaCola$StockPrice)

# median of Boeing over this time
median(Boeing$StockPrice)

# standard deviation of Procter & Gamble
sd(ProcterGamble$StockPrice)


#
# Part 2
#


#
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

# add vertical line
abline(v=as.Date(c("2000-03-01")), lwd=2)

abline(v=as.Date(c("1983-03-01")), lwd=2)


# 
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="blue2", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="green", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="coral", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="plum", ylim=c(0,210))

# During which months is IBMs average stock price higher than the overall average
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean, na.rm=TRUE)

#
tapply(Boeing$StockPrice, months(Boeing$Date), mean, na.rm=TRUE)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean, na.rm=TRUE)
tapply(GE$StockPrice, months(GE$Date), mean, na.rm=TRUE)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean, na.rm=TRUE)