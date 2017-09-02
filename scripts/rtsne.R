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

# Creating Rtsne Feature
library(Rtsne)


# Categorical features

one_hot <- model.matrix(~., data = categorical_df)
one_hot <- as.data.table(one_hot)

cat_vars <- nzv(one_hot)

one_hot <- one_hot[,-c(colnames(one_hot)[cat_vars]),with=F]

tsne<- Rtsne(as.matrix(one_hot),check_duplicates = F,pca = F,perplexity = 30,theta = 0.5, dims = 2)
head(tsne$Y)

tsne_feat <- as.data.table(tsne$Y)

train_feat <- tsne_feat[1:nrow(train)]
test_feat <- tsne_feat[(nrow(train)+1):nrow(categorical_df)]

setnames(train_feat,c('V1','V2'),c('cat1','cat2'))
setnames(test_feat,c('V1','V2'),c('cat1','cat2'))

fwrite(train_feat,"features/rtsne_cat_train.csv")
fwrite(test_feat,"features/rtsne_cat_test.csv")

rm(train_feat,test_feat,tsne_feat)
gc()

# Binary features

def_feat <- nzv(binary_df)
binary_df <- binary_df[,-c(colnames(binary_df)[def_feat]),with=F]

tsne<- Rtsne(as.matrix(binary_df),check_duplicates = F,pca = F,perplexity = 30,theta = 0.5, dims = 2)
head(tsne$Y)

tsne_feat <- as.data.table(tsne$Y)

train_feat <- tsne_feat[1:nrow(train)]
test_feat <- tsne_feat[(nrow(train)+1):nrow(categorical_df)]

setnames(train_feat,c('V1','V2'),c('bin1','bin2'))
setnames(test_feat,c('V1','V2'),c('bin1','bin2'))

fwrite(train_feat,"features/rtsne_bin_train.csv")
fwrite(test_feat,"features/rtsne_bin_test.csv")



















