### Group3/lab1_rapart1
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

### Group3/lab1_rapart2
# Regression Tree Example
# build the  tree
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)

### Group3/lab1_rpart3
library(e1071)
library(rpart)
library(mlbench)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)

### Group3/lab1_rpart4
fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) # display the results
plotcp(fitK) # visualize cross-validation results
summary(fitK) # detailed summary of splits
# plot tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree
post(fitK, file = 'kyphosistree.ps',title = "Classification Tree for Kyphosis") # might need to convert to PDF (distill)

pfitK<- prune(fitK, cp=fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])

plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
post(pfitK, file = 'kyptree.ps', title = 'Pruned Classification Tree for Kyphosis') 


### Group3/lab1_randomforest1
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)
getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) 

# look at rfcv - random forest cross-validation - 
help(rfcv)

### Titanic
library(party)
model_rpart <- rpart(Survived ~ .,data=Titanic)
model_ctree <- ctree(Survived ~ .,data=Titanic)
model_randomforest <- randomForest(Survived ~ .,data=Titanic)
model_hclust <- hclust(dist(Titanic))

### SVM
library(e1071)
set.seed (1)

x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
# We begin by checking whether the classes are linearly separable.
plot(x, col=(3-y))

dat <- data.frame(x = x,y  = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
summary(svmfit)
plot(svmfit , dat)

svmfit$index


svmfit1 <- svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit1 , dat)
svmfit$index


set.seed (1)
tune.out <- tune(svm, y ~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

summary(tune.out)

bestmod=tune.out$best.model 
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

ypred <-predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat=data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)

svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)


library(e1071)
library(ISLR)
names(Khan)

dim(Khan$xtrain )
dim(Khan$xtest )

length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

dat <- data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
out <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

### Group3/ab1_svm1
n <- 150 
p <- 2 # dimension
sigma <- 1 # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples

# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)

# Generate the labels
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1

plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))


### Karatzoglou
library("kernlab")
data("iris")
irismodel <- ksvm(Species ~ ., data = iris,type = "C-bsvc", kernel = "rbfdot",kpar = list(sigma = 0.1), 
                  C = 10, prob.model = TRUE)
irismodel

predict(irismodel, iris[c(3, 10, 56, 68,107, 120), -5], type = "probabilities")
predict(irismodel, iris[c(3, 10, 56, 68,107, 120), -5], type = "decision")

k <- function(x, y) {(sum(x * y) + 1) * exp(0.001 * sum((x -y)^2))}

class(k) <- "kernel"
data("promotergene")
gene <- ksvm(Class ~ ., data = promotergene,kernel = k, C = 10, cross = 5)
gene

x <- rbind(matrix(rnorm(120), , 2), matrix(rnorm(120,mean = 3), ,2))
y <- matrix(c(rep(1, 60), rep(-1, 60)))
svp <- ksvm(x, y, type = "C-svc", kernel = "rbfdot",kpar = list(sigma = 2))
plot(svp)

library("klaR")
data("B3")
Bmod <- svmlight(PHASEN ~ ., data = B3,svm.options = "-c 10 -t 2 -g 0.1 -v 0")
predict(Bmod, B3[c(4, 9, 30, 60, 80, 120),-1])

library("svmpath")
data("svmpath")
attach(balanced.overlap)
svmpm <- svmpath(x, y, kernel.function = radial.kernel,param.kernel = 0.1)
predict(svmpm, x, lambda = 0.1)
