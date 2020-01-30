library(ISLR)
data('Auto')
head(Auto)
tail(Auto)
names(Auto) # the name of coloumns
summary(Auto)
summary(Auto$mpg) # choose a coloumn
fivenum(Auto$mpg)
boxplot(Auto$mpg)

help("read.csv")
data1 <- read.csv(file.choose(),header = TRUE) # it will pop a window to choose data
data1
is.na(data1)
hist(Auto$mpg)
attach(Auto) 

str(Auto) # the information of the coloumns such as type

library('MASS')
data('Boston')
names(Boston)
summary(Boston)
attach(Boston)
hist(zn)
str(Boston)
summary(crim)

summary(data1$EPI)
boxplot(data1$EPI)