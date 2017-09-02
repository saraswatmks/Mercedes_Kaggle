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

train_id <- train$ID
train_labels <- train$y
test_id <- test$ID

train[,c('ID','y') := NULL]
test[,ID := NULL]

df_all <- rbind(train,test)

categorical_vars <- paste0('X',c(0:6,8))
categorical_df <- df_all[,categorical_vars,with=F]
binary_df <- df_all[,-categorical_vars, with=F]

# # dropping columns with 0 standard deviation
# cols_to_drop <- c('X11', 'X93', 'X107', 'X233', 'X235', 'X268', 'X289', 'X290', 'X293', 'X297', 'X330','X347')
# binary_df <- binary_df[, -cols_to_drop, with=F]

# categorical SVD

one_hot <- model.matrix(~.-1, data = categorical_df)
one_hot <- as.data.table(one_hot)

cat_vars <- nzv(one_hot)

one_hot <- one_hot[,-c(colnames(one_hot)[cat_vars]),with=F]

library(rARPACK)

tsvd <- svds(A = as.matrix(one_hot)
             ,k = 20
             ,nu = 5
             ,nv = 5
             )
plot(cumsum(tsvd$d^2/sum(tsvd$d^2)))

pc.use <- 1
recon <- tsvd$u[,pc.use] %*% diag(tsvd$d[pc.use], length(pc.use), length(pc.use)) %*% t(tsvd$v[,pc.use])
recon <- as.data.table(recon)

## SVD is same as PCA - Stop Here



