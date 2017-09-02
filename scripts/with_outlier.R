path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)

#### load
library(data.table)
library(e1071)

train <- fread("train.csv")
test <- fread("test.csv")

# skewness(train$y)
# train[,summary(y)]
# train <- train[y < 260]
# skewness(train$y)

train_id <- train$ID
train_labels <- train$y
test_id <- test$ID

train[,c('ID','y') := NULL]
test[,ID := NULL]

df_all <- rbind(train,test)

categorical_vars <- paste0('X',c(0:6,8))
categorical_df <- df_all[,categorical_vars,with=F]
binary_df <- df_all[,-categorical_vars, with=F]


# Use only Binary Data ----------------------------------------------------

# split binary data into train and test
train_binary <- binary_df[1:nrow(train)]
test_binary <- binary_df[(nrow(train)+1):nrow(binary_df)]

for ( x in colnames(binary_df))
  set(x = train_binary, j = x, value = as.numeric(train_binary[[x]]))

for ( x in colnames(test_binary))
  set(x = test_binary, j = x, value = as.numeric(test_binary[[x]]))


library(MLmetrics)
rsquare <- function(preds, dtrain){
  
  label <- getinfo(dtrain, 'label')
  err <- R2_Score(preds, label)
  return(list(metric = 'r_squared',value = err))
  
}


# Taking target variable with missing value with log

library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(train_binary), label = log1p(train_labels))
dtest <- xgb.DMatrix(data = as.matrix(test_binary))

params <- list(
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  colsample_bytree = 1,
  subsample = 1
  
)

bst.cv <- xgb.cv(params = params
                 ,data = dtrain
                 ,nrounds = 500
                 ,nfold = 5L
                 ,feval = rsquare
                 ,early_stopping_rounds = 20
                 ,maximize = T
                 ,print_every_n = 10)

bst <- xgb.train(params = params
                 ,data = dtrain
                 ,nrounds = 68)


bst.pred <- predict(bst, dtest)
base_sub <- data.table(ID = test_id, y = exp(bst.pred))
fwrite(base_sub, "xgboost_binaryWO_25June.csv") #LB 0.54425 | #CV 0.623984



# Next Model, Binary + Categorical Variables ------------------------------

onehot_cat <- model.matrix(~.-1, data = categorical_df)
onehot_cat <- as.data.table(onehot_cat)

train_cat <- onehot_cat[1:nrow(train)]
test_cat <- onehot_cat[(nrow(train)+1):nrow(onehot_cat)]

train_cat <- cbind(train_cat, train_binary)
test_cat <- cbind(test_cat, test_binary)

library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(train_cat), label = log1p(train_labels))
dtest <- xgb.DMatrix(data = as.matrix(test_cat))

params <- list(
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  colsample_bytree = 1,
  subsample = 1
  #gamma = 2
  
)

bst.cv <- xgb.cv(params = params
                 ,data = dtrain
                 ,nrounds = 500
                 ,nfold = 5L
                 ,feval = rsquare
                 ,early_stopping_rounds = 20
                 ,maximize = T
                 ,print_every_n = 10)

bst <- xgb.train(params = params
                 ,data = dtrain
                 ,nrounds = 66)


bst.pred <- predict(bst, dtest)
base_sub <- data.table(ID = test_id, y = exp(bst.pred)-1)
fwrite(base_sub, "xgboost_binarywithcats_25June.csv") #LB 0.54708 | #CV 0.635544
