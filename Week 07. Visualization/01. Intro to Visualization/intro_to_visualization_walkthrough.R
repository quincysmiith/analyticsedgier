#############
#
# Analytics Edge
# intro to viz walkthrough
#
#############

# at work
setwd("C:/Users/marquin.smith/Downloads")

who <- read.csv("who.csv")
str(who)

plot(who$GNI, who$FertilityRate)

library(ggplot2)

scatterplot <- ggplot(who, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()

# we could use a line if we wanted
scatterplot + geom_line()

# a scatter plot is better
scatterplot + geom_point()
scatterplot + geom_point(color="blue", size = 3, shape=17)
scatterplot + geom_point(color="darkred", size = 3, shape=8)

# Add title
scatterplot + geom_point(color="darkred", size = 3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")

# we can save this to a file
fertilityGNIplot <- scatterplot + geom_point(color="darkred", size = 3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")
pdf("myplot.pdf")
print(fertilityGNIplot)

# close the file
dev.off()


#
# Quick Question
#

scatterplot + geom_point(color="darkred", size = 3, shape=15)


### Advanced  scatterplots using ggplot

# adding a color per region in the scatterplot
ggplot(who, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()


ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# are fertility rate and % under 15 years related?
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()

# looks like a relationship but doesn't look linear.
# lets take the log of fertility rate

ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()
# this looks more linear now

model <- lm(Under15 ~ log(FertilityRate), data = who)
summary(model)

# add regression line to plot
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm")

ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", level=0.99)

#remove confidence intervals
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE)

# change colour of regression line
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE, color="orange")


#
# Quick quiz
#

ggplot(who, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point(size = 5) +scale_color_brewer(palette="Dark2")
