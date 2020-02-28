library(rpart)
library(rpart.plot)
library(ggplot2)
data("msleep")
str(msleep)
help("msleep") # read the documentation for the msleep dataset.it is about mammals sleep dataset
# observe the structure of the #msleep dataset

# creating a new data frame with the following columns included.
mSleepDF1 <- msleep[,c(3,6,10,11)] 
# 3 = vore ,6=sleep_total, 10=brainwt, 11=bodywt
# observe the structure of the mSleepDF 
str(mSleepDF1)
head(mSleepDF1)

# Building Regression Decision Tree that #predicts the total sleeping
# hours of the mamals based on the other #variables available on the dataset
sleepModel_1 <- rpart(sleep_total ~ ., data=mSleepDF1, method = "anova")
# method we are using here is anova becuase our target here is sleep_total is a numerical one.
sleepModel_1
help(rpart)

# let's visualize this using rpart.plot()
help("rpart.plot")
rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)
# type = 3, Draw separate split labels for the left and right directions.See the documentation 
#fallen.leaves = TRUE,  Default TRUE to position the leaf nodes at the bottom of the graph. 
#It can be helpful to use FALSE if the graph is too crowded and the text size is too small.
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves = TRUE) # with 3 digits 
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves = TRUE)

# iris dataset
data(iris)
head(iris)
str(iris)
library(C50)
set.seed(9850)
# generate random numbers
grn <- runif(nrow(iris))
help(runif)
irisrand <- iris[order(grn),]
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
help(C5.0)

prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1

table(irisrand[101:150,5],prediction1)
plot(classificationmodel1)

library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 


