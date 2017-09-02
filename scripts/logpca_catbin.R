rm(list=ls())

path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)


# With removing outliers --------------------------------------------------

library(data.table)
library(e1071)

train <- fread("train.csv")
test <- fread("test.csv")

skewness(train$y)
train[,summary(y)]

train <- train[y < 260]

skewness(train$y)

train_id <- train$ID
train_labels <- train$y
test_id <- test$ID

train[,c('ID','y') := NULL]
test[,ID := NULL]

df_all <- rbind(train,test)

categorical_vars <- paste0('X',c(0:6,8))
categorical_df <- df_all[,categorical_vars,with=F]

one_hot <- model.matrix(~.-1, data = categorical_df)
one_hot <- as.data.table(one_hot)

sapply(one_hot, function(x) sd(x))

# remove nzv faetures
library(caret)

nzv <- nzv(one_hot)
colnames(one_hot)[nzv]

# remove constant features
one_hot <- one_hot[,-c(colnames(one_hot)[nzv]),with=F]


# get binary variables
binary_df <- df_all[,-categorical_vars, with=F]

nzv <- nzv(binary_df)
length(nzv)

binary_df <- binary_df[,-c(colnames(binary_df)[nzv]),with=F]


# join the data

alldata <- cbind(one_hot, binary_df)

# split the data

train_dt <- alldata[1:nrow(train)]
test_dt <- alldata[(nrow(train)+1):nrow(alldata)]

k <- 20
m <- 15
logpca_model <- logisticPCA(train_dt, k = k, m = m)
logpca_model
logpca_features <- logpca_model$PCs

logpca_features <- as.data.table(logpca_features)
colnames(logpca_features) <- paste0('LPCB',1:k)

logpca_features_test <- predict(logpca_model, test_dt)
logpca_features_test <- as.data.table(logpca_features_test)
colnames(logpca_features_test) <- paste0('LPCB',1:k)

fwrite(logpca_features,"features/logpca_catbin_train.csv")
fwrite(logpca_features_test,"features/logpca_catbin_test.csv")















