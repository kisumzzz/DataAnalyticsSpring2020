# import required packages
library(corrplot)
library(ggplot2)
library(GGally)
library(dummies)
library(forecast)
library(dplyr)
library(plm)
library(zoo)
library(dummies)
library(pglm)
library(lme4)
library(broom)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(gains)
library(class)
library(reshape2)
library(knitr)

# import data from local folder
train.failedBanks <- read.csv("/Users/kisumzzz/Desktop/fin/21failbanks.csv",sep = ",",header = TRUE)
train.activeBanks <- read.csv("/Users/kisumzzz/Desktop/fin/21activebank.csv",sep = ",",header = TRUE)

valid.failedBanks <- read.csv("/Users/kisumzzz/Desktop/fin/2017bankfail.csv", sep = ",", header = TRUE)
valid.activeBanks <- read.csv("/Users/kisumzzz/Desktop/fin/2017active.csv", sep = ",", header = TRUE)

# merge datasets into a train data set and a validation data set
train <- rbind(train.activeBanks,train.failedBanks)
dim(train)

# make outcome variables for validation dataset
valid.failedBanks$fail <- 1
valid.failedBanks$failyear <- 2017

valid.activeBanks$fail <- 0
valid.activeBanks$failyear <- NA

valid <- rbind(valid.activeBanks, valid.failedBanks)
dim(valid)

# merge train data and validation data to a whole data set
BankFailure.Data <- rbind(train,valid)
dim(BankFailure.Data)

# View the raw data in seperate window
View(BankFailure.Data)

# Check the size of each data set
head(train)
dim(train)
## [1] 840  51

head(valid)
dim(valid)
## [1] 200  51

df <- BankFailure.Data

# filter all numeric variables to make descriptive statistics
# "name" "city" "stalp" "repdte" "bkclass" were excluded
data.numeric <- Filter(is.numeric,df)
dim(data.numeric)
## 1040   46

# check missing values of all numeric variables
NAs <- sapply(data.numeric, function(x) sum(is.na(x)))
NAs

# fill the missing values
# there exist NAs in rssdhcr(50), hctmult(189), failyear(520), 
# igltrad(11), netinbm (434), astempm(1), rbc1aaj(1), rbc1rwaj(1), rbcrwaj(2)

# 1. fill the missing values in rssdhcr with all zero
df$rssdhcr[is.na(df$rssdhcr)] <- 0

# 2. fill the missing values in hctmult with all zero
# because there exist 829 zeros in the data with 22 ones
df$hctmult[is.na(df$hctmult)] <- 0

# 3. we will not use failyear in our model, since it is just a index for information

# 4. fill the missing values in igltrad with mean value
df$igltrad[is.na(df$igltrad)] <- mean(df$igltrad, na.rm = TRUE)

# 5. we will drop feature netinbm, since it contains almost half of NAs

# 6. there exists NA value in bank with cert ID 29730 on 06/30/08, so i fill it with data on 03/31/08
df$astempm[is.na(df$astempm)] <- 4.963686773

# 7. Fill the NAs of rbc1aaj(1), rbc1rwaj(1), rbcrwaj(2)
# fill the missing value in rbcrwa for bank cert id 19450 with last quater data
df$rbcrwaj[is.na(df$rbcrwaj)&&(df$cert==19450)] <- 4.548076923

# * Three missing values come from bank with Cert ID 15062 on report date 3/31/14	
# * I will the missing values with number on 12/31/13
df$rbc1aaj[is.na(df$rbc1aaj)] <- 0.410677618
df$rbc1rwaj[is.na(df$rbc1rwaj)] <- 0.840551109
df$rbcrwaj[is.na(df$rbcrwaj)] <- 1.681102218

# Descriptive data analysis
data.numeric <- Filter(is.numeric,df)
dim(data.numeric)

# double check missing values of all numeric variables
NAs <- sapply(data.numeric, function(x) sum(is.na(x)))
NAs

# make descriptive statistics
data.numeric.summary <- summary(data.numeric)
data.numeric.summary
#write.csv(data.numeric.summary,file = "descriptive statistics.csv",row.names = FALSE)

# Aggregate statistics 
BankFailure.Data %>%
  group_by(fail, cert) %>%
  summarize(mean_asset = mean(asset, na.rm = TRUE), 
            mean_loans_leases = mean(lnlsnet,na.rm = TRUE),
            mean_Volatile_liabilities = mean(voliab, na.rm = TRUE),
            mean_interest_income = mean(intinc, na.rm = TRUE),
            mean_interest_expense = mean(eintexp, na.rm = TRUE),
            mean_YieldOnEarningAssets = mean(intincy, na.rm = TRUE)
  ) %>%
  {. ->> output } %>%   #here is save
  tbl_df %>% 
  print(n = Inf)

# data visualization
# drop variables
data.numeric.afterdropped <- subset(data.numeric, select = -c(netinbm, failyear,cert, rssdhcr))

# evaluate the correlation between variables
# draw a heatmap
corrplot(cor(data.numeric.afterdropped),method="shade")

# calculate correlation coefficients
corr = cor(data.numeric.afterdropped)
# write.csv(corr,file = "correlation coefficients.csv",row.names = TRUE)

# calculate correlation coefficients between fail and other numeric independent variables
corr.fail = as.data.frame(t(cor(data.numeric.afterdropped$fail,data.numeric.afterdropped[,1:41])))
# write.csv(corr.fail,file = "corr with fail indicator.csv",row.names = TRUE)

# calculate statistis for full list after dropping variables
mean <- sapply(data.numeric,mean)
min <- sapply(data.numeric,min)
max <- sapply(data.numeric,max)
NAs <- sapply(data.numeric, function(x) sum(is.na(x)))

stat <- cbind(mean,min,max,NAs)

# write.csv(stat,file = "stat.csv",row.names = TRUE)

# Additional EDA on panel data
# Explore the data using plm panel data package

# drop unrelated features from panel data frame
df.model.initial <- subset(df, select = -c(cert, rssdhcr, repdte, eintexp, elnatr, igltrad, iglsec, eqcdiv, intan, failyear,docket,fed_rssd, name, stalp, zip, city, netinbm, bkclass, hctmult, subnd, iglsec, eqcdiv))

# create dummies for variable bkclass
df.model.dummies <- dummy.data.frame(df.model.initial, sep = "_", drop = TRUE)

# create date id to make a balanced panel 
dateid <- as.data.frame(rep(1:20,52))

df.model <- cbind(df[,1], dateid, df.model.dummies)

names(df.model)[1] <- "cert"
names(df.model)[2] <- "dateid"

# build the panel data frame
# Two further arguments are logical: 
# drop.index = TRUE drops the indexes from the data.frame，and row.names = TRUE computes “fancy” row names by pasting the individual and the time indexes. 
df.p <- pdata.frame(df.model, index = c("cert", "dateid"), drop.index = TRUE, row.names = TRUE)
# write.csv(df.p,file = "Panel Data.csv",row.names = TRUE)

# view the panel index combined with cert ID and assigned date id
head(attr(df.p, "index"))

# get summary of one feature
summary(df.p$asset)
summary(df.p$chbal)
summary(df.p$lnlsnet)
summary(df.p$lnatres)

# evaluate the between effect 
head(between(df.p$asset), 52)

# Panel Data Evaludation using pglm package
df.p.names <- names(df.p)

# write the formula
f <- as.formula(paste("fail ~", paste(df.p.names[!df.p.names %in% "fail"], collapse = " + ")))
f
# Oneway effect - Pooling Model
pl <- plm(f, df.p, model = "pooling")

summary(pl)
pl$

# Panel Data Evaludation using lme4 package
# Mixed effects logistic regression
# write the formula

me <- glmer(fail~rbct2 + intinc + nonii + idothnii + nonix + idpretx + intincy + roa + rbcrwaj + rbc1rwaj  + (1|cert) + (1|dateid), data = df.model, family = binomial)
me

# Given the mixed effects, the effect of time can be eliminated, we can ignore the effect of panel data and use normal ML algothms
# Train-Validation Split
# drop related variables
df.model.norm <- subset(df, select = -c(cert, repdte, failyear,docket,fed_rssd, name, zip, city, netinbm, rssdhcr))

# create dummies for variable bkclass and stalp
df.model.norm.all <- dummy.data.frame(df.model.norm, sep = "_", drop = TRUE)
df.model.norm.all

# We use 21 active banks and 12 failed banks from 2007 to 2016 as train data.
# We use 5 active banks and 5 failed banks in 2017 as validation data.
# For each bank, we collect 5 years or 20 quarters data.
train <- df.model.norm.all[1:840, ]
valid <- df.model.norm.all[841:1040, ]

################ Decision Tree Model ###################

# Best Pruned Tree_ the smallest tree within one standard error of the minimum cross-validation error
cv.ct <- rpart(fail ~ ., data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5,minbucket = 50, maxdepth = 7)

printcp(cv.ct)

# Plot the best pruned tree
pruned.ct <- prune(cv.ct, cp = 0.028571)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))
plotcp(pruned.ct)

# Predictions on validation set
pred <- predict(pruned.ct, valid, type = "class")

# Confusion matrix
confusionMatrix(pred, as.factor(valid$fail))

################ kNN ###################
# Check the correlation with BIN
train_x_knn <- train[,1:71]
train_y_knn <- train$fail

test_x_knn <- valid[,1:71]
test_y_knn <- valid$fail

# there are 840 observations in the train data set.
# square root of 749 is 28.98
knn.28 <- knn(train=train_x_knn, test=test_x_knn, cl=train_y_knn, k=28)
knn.29 <- knn(train=train_x_knn, test=test_x_knn, cl=train_y_knn, k=29)

# Evaluate hte performance
#Calculate the proportion of correct classification for k = 28, 29
ACC.28 <- sum(as.factor(test_y_knn) == knn.28)/NROW(test_y_knn)
ACC.28
ACC.29 <- sum(as.factor(test_y_knn) == knn.29)/NROW(test_y_knn)
ACC.29

# optimization of k-value
i=1
k.optm=1
for (i in 1:200){
  knn.mod <- knn(train=train_x_knn, test=test_x_knn, cl=train_y_knn,k=i)
  k.optm[i] <- sum(as.factor(test_y_knn) == knn.mod)/NROW(test_y_knn)
  k=i
  cat(k,'=',k.optm[i],'
      ')
}

# Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

# choose the value of k that maximize the accuracy
k.star <- which.max(k.optm)
sprintf("Optimal value of k is %1$d with validation accuracy of %2$.7f",k.star, max(k.optm))
# [1] "Optimal value of k is 39 with validation accuracy of 0.7350000"

# Finalize the model of kNN
knn <- knn(train=train_x_knn, test=test_x_knn, cl=train_y_knn, k=39)
knn

# Confusion matrix
confusionMatrix(knn, as.factor(valid$fail))

################ logistic Regression ###################
# fit the model using train data
LR.model <- glm(fail ~ ., data = train, family=binomial)
col <- c('stalp_SC','stalp_MN','stalp_WI','stalp_KS','stalp_NJ')

# summary of model
summary(LR.model)

# make predictions for test data
valid$model_prob <- predict(LR.model, valid, type = "response")

# Calculate accuracy
valid <- valid  %>% mutate(model_pred = 1*(model_prob > 0.53) + 0)
valid <- valid %>% mutate(accurate = 1*(model_pred == fail))
valid
LR.accuracy <- sum(valid$accurate)/nrow(valid)
LR.accuracy
# 0.65

# optimization of thresold setting
threshold <- seq(0.50,0.99,by=0.01)
i=1
LR.accuracy <- rep(NA, length(threshold))

for (t in threshold){
  valid <- valid  %>% mutate(model_pred = 1*(model_prob > t) + 0)
  valid <- valid %>% mutate(accurate = 1*(model_pred == fail))
  
  LR.accuracy[i] <- sum(valid$accurate)/nrow(valid)
  i = i+1
}

################ Ensemble Models ###################
################ bagged tree model ###################
# Fit a bagged tree model to the data to predict failure.  
# Comment on the optimal hyperparameters 
x <- subset(df.model.norm.all, select=c(-fail))
y <- as.factor(df.model.norm.all$fail)

# 5 fold cross validation:
fitControl <- trainControl(method = "cv",number=5)
#fitControl <- trainControl(method = "LOOCV")

# Set the tuning parameters:
grid <- expand.grid(.mtry=ncol(x), .splitrule="gini",
                    .min.node.size=5)

# method="ranger",
bag.treebag <- train(x=x,y=y, method="ranger",
                     trControl = fitControl,
                     metric="Accuracy",
                     tuneGrid=grid,
                     num.trees=10)

print(bag.treebag)

names(bag.treebag)

# check the final model
print(bag.treebag$finalModel)

# check the optimal tuning hyperparameters
print(bag.treebag$bestTune)

################ Random Forest model ###################
b <- c(1,seq(10,100,by=10))
m <- c(2:ncol(x))
oob.error.rf <- matrix(NA,ncol=length(m),
                       nrow=length(b))
rf.treebag <- list(NA) 
nn <- 1

for (j in 1:length(m)){
  grid <- expand.grid(.mtry=m[j], .splitrule="gini",
                      .min.node.size=5) 
  for (i in 1:length(b)){
    rf.treebag[[nn]] <- train(x=x,y=y, method="ranger",
                              trControl = fitControl,
                              metric="Accuracy",
                              tuneGrid=grid,
                              num.trees=b[i])
    oob.error.rf[i,j] <- rf.treebag[[nn]]$finalModel$prediction.error 
    print(i)
    nn <- nn + 1
    print(nn) 
  }
}

for (i in 1:length(rf.treebag)) {
  print(rf.treebag[[i]]$bestTune)
  print(rf.treebag[[i]]$results)
}

rf.treebag[[1]]$results

df.plot <- data.frame(b,oob.error.rf) 
names.tmp <- c("b")
for (i in 1:length(m)) {
  names.tmp[i+1] <- paste0("mtry.",m[i]) 
}

names(df.plot) <- names.tmp
melt.df.plot <- melt(df.plot,id="b")
p <- ggplot(melt.df.plot, aes(x=b,y=value,color=variable)) + geom_line()
print(p)

################ Gradient Boosting model ###################
# Fit a boosted tree to the data to predict bank failure. 
# Comment on the optimal hyperparameters you find (number of trees, learning rate, etc.).
fitControl <- trainControl(method = "cv",number=5)

# Convert character vars to factors
for (i in 1:ncol(x)) { 
  print(class(x[,i]))
}

for (i in 1:ncol(x)) {
  print(class(x[,i])) 
}

# Set the tuning parameters:
grid <- expand.grid(.n.trees=c(100,1000,5000), 
                    .interaction.depth=c(1:4),
                    .shrinkage=c(0.001,0.1,0.5), 
                    .n.minobsinnode=5)

boost.gbm <- train(x=x,y=y, method="gbm",
                   trControl = fitControl,
                   metric="Accuracy",
                   tuneGrid=grid)

summary(boost.gbm) 

results.gbm <- boost.gbm$results
kable(as.data.frame(results.gbm),digits=c(4,0,0,0,4,4,4,4),format.args = list(big.mark=","))
