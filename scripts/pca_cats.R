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

head(categorical_df)

one_hot <- model.matrix(~.-1, data = categorical_df)
one_hot <- as.data.table(one_hot)

sapply(one_hot, function(x) sd(x))

# remove nzv faetures
library(caret)

nzv <- nzv(one_hot)
colnames(one_hot)[nzv]

# remove constant features
one_hot <- one_hot[,-c(colnames(one_hot)[nzv]),with=F]

train_cats <- one_hot[1:nrow(train)]
test_cats <- one_hot[(nrow(train)+1):nrow(one_hot)]

tpca <- prcomp(train_cats, scale. = T)

var_pca <- (tpca$sdev)^2
tot_sum <- var_pca/sum(var_pca)

sum(tot_sum[1:30])

train_pca <- as.data.table(tpca$x[,1:30])

testpca <- predict(tpca, test_cats)
testpca <- as.data.table(testpca[,1:30])

fwrite(train_pca, "features/pca_cats_train.csv")
fwrite(testpca, "features/pca_cats_test.csv")
















