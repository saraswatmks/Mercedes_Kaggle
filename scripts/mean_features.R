rm(list=ls())

path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)


#### load
library(data.table)
library(e1071)

train <- fread("train.csv")
test <- fread("test.csv")

skewness(train$y)
train[,summary(y)]
train <- train[y < 260]
skewness(train$y)

vars <- paste0('X',c(0:6,8))
train_use <- train[,c(vars,'y','ID'),with=F]
test_use <- test[,c(vars,'ID'),with=F]

head(train_use)

sapply(train_use[,vars,with=F], function(x) length(unique(x)))

var_nam <- paste0("mean",vars)

# for(x in seq(vars)){
#   new_train <- train_use[,var_nam[[x]] := mean(y), eval(vars[[x]])]
# }
  
mean0 <- train_use[,mean(y),"X0"]
setnames(mean0,"V1","meanX0")

mean1 <- train_use[,mean(y),"X1"]
setnames(mean1,"V1","meanX1")

mean2 <- train_use[,mean(y),"X2"]
setnames(mean2,"V1","meanX2")

mean5 <- train_use[,mean(y),"X5"]
setnames(mean5,"V1","meanX5")

mean6 <- train_use[,mean(y),"X6"]
setnames(mean6,"V1","meanX6")

mean8 <- train_use[,mean(y),"X8"]
setnames(mean8,"V1","meanX8")

train_use <- mean0[train_use, on="X0"]
train_use <- mean1[train_use, on="X1"]
train_use <- mean2[train_use, on="X2"]
train_use <- mean5[train_use, on="X5"]
train_use <- mean6[train_use, on="X6"]
train_use <- mean8[train_use, on="X8"]

test_use <- mean0[test_use, on="X0"]
test_use <- mean1[test_use, on="X1"]
test_use <- mean2[test_use, on="X2"]
test_use <- mean5[test_use, on="X5"]
test_use <- mean6[test_use, on="X6"]
test_use <- mean8[test_use, on="X8"]

remov_vars <- paste0("X",c(0:4,5,6,8))

train_use[,(remov_vars) := NULL]
test_use[,(remov_vars) := NULL]

fwrite(train_use,"features/mean_feats_train.csv")
fwrite(test_use,"features/mean_feats_test.csv")









