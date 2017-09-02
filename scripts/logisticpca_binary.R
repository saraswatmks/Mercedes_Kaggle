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
#categorical_df <- df_all[,categorical_vars,with=F]
binary_df <- df_all[,-categorical_vars, with=F]

# # dropping variables with near zero standard deviation or variance
# sapply(binary_df[,cols_to_drop,with=F], function(x) sd(x))
# 
# # dropping columns with 0 standard deviation
# cols_to_drop <- c('X11', 'X93', 'X107', 'X233', 'X235', 'X268', 'X289', 'X290', 'X293', 'X297', 'X330','X347')
# binary_df <- binary_df[, -cols_to_drop, with=F]

library(caret)

nzv <- nzv(binary_df)
nzv

binary_df <- binary_df[,-c(colnames(binary_df)[nzv]),with=F]

head(binary_df)

train_binary <- binary_df[1:nrow(train)] 
test_binary <- binary_df[(nrow(train)+1):nrow(binary_df)]

library(rARPACK)
library(logisticPCA)

k <- 20
m <- 12
logpca_model <- logisticPCA(train_binary, k = k, m = 15)
logpca_model
logpca_features <- logpca_model$PCs

logpca_features <- as.data.table(logpca_features)
colnames(logpca_features) <- paste0('LPC',1:k)

logpca_features_test <- predict(logpca_model, test_binary)
logpca_features_test <- as.data.table(logpca_features_test)
colnames(logpca_features_test) <- paste0('LPC',1:k)

fwrite(logpca_features,"features/logpca_binary_train.csv")
fwrite(logpca_features_test,"features/logpca_binary_test.csv")











