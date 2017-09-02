path <- "/home/manish/Desktop/Data2017/June/Mercedes/features/"
setwd(path)


# Creating data -----------------------------------------------------------

library(data.table)

myfiles <- list.files(path = path, pattern = "*.csv")

trainfiles <- grep(pattern = "train",x = myfiles, value = T)
testfiles <- grep(pattern = "test",x = myfiles, value = T)

trainfiles <- setdiff(trainfiles,c("train_onehot_embedding.csv","train_integer_embedding.csv"))
testfiles <- setdiff(testfiles,c("test_ica.csv","test_onehot_embedding.csv","test_integer_embedding.csv"))
testfiles

# combining train data

for(x in seq(trainfiles)){
  
  assign(paste0("train",x), fread(trainfiles[[x]]))
  
}


alltrain <- cbind(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10,
                  train11, train12, train13, train14, train15, train16, train17, train18, train19,
                  train20)

sel_cols <- setdiff(colnames(alltrain),c("ID","V1"))

alltrain <- alltrain[,sel_cols,with=F]

fwrite(alltrain,"alltrain.csv")

rm(list=ls())

# combining test data

for(x in seq(testfiles)){
  
  assign(paste0("test",x), fread(testfiles[[x]]))
  
}

alltest <- cbind(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10,
                 test11, test12, test13, test14, test15, test16, test17, test18, test19,
                 test20)

sel_cols <- setdiff(colnames(alltest), c('ID','V1'))

alltest <- alltest[,sel_cols, with=F]

fwrite(alltest,"alltest.csv")

rm(list=ls())



# loading data and training xgboost ---------------------------------------

train <- fread('alltrain.csv')
test <- fread('alltest.csv')

target <- log1p(train$y)

train <- train[,-c('y'),with=F]

# add ID feature in train and test
train_id <- fread("../train.csv", select = c('ID','y'))
train_id <- train_id[y < 200]

test_id <- fread("../test.csv",select = 'ID')

train <- cbind(train, train_id$ID)
test <- cbind(test,test_id$ID)


library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(train), label = target)
dtest <- xgb.DMatrix(data = as.matrix(test))

library(MLmetrics)
rsquare <- function(preds, dtrain){
  
  label <- getinfo(dtrain, 'label')
  err <- R2_Score(preds, label)
  return(list(metric = 'r_squared',value = err))
  
}

params <- list(
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 5,
  alpha=10,
  colsample_bytree = 0.5,
  subsample = 0.85
  
)

set.seed(101)

bst.cv <- xgb.cv(params = params
                 ,data = dtrain
                 ,nrounds = 500
                 ,nfold = 10L
                 ,feval = rsquare
                 ,early_stopping_rounds = 20
                 ,maximize = T
                 ,print_every_n = 10)

bst.train <- xgb.train(params = params, data = dtrain, nrounds = 176)

imp <- xgb.importance(feature_names = colnames(dtrain),model = bst.train)
xgb.plot.importance(imp,top_n = 30)

pred <- predict(bst.train, dtest)
pred[1:10]

pred <- exp(pred)-1


sub <- data.table(ID = test_id$ID, y = pred)
fwrite(sub,"sub1.csv") #0.14
fwrite(sub,"sub2.csv") #with ID feature #0.30193

# create a model on top 200, 100, 50 features
top150 <- imp$Feature[1:150]  #0.632516 CV | LB 0.53560 
top100 <- imp$Feature[1:100] #0.632691 | LB 0.54102
top50 <- imp$Feature[1:50] #0.633896 | 

dtrain <- xgb.DMatrix(data = as.matrix(train[,top50,with=F]), label = target)
dtest <- xgb.DMatrix(data = as.matrix(test[,top50,with=F]))

library(MLmetrics)
rsquare <- function(preds, dtrain){
  
  label <- getinfo(dtrain, 'label')
  err <- R2_Score(preds, label)
  return(list(metric = 'r_squared',value = err))
  
}

params <- list(
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 5,
  alpha=10,
  colsample_bytree = 0.5,
  subsample = 0.85
  
)

set.seed(101)

bst.cv <- xgb.cv(params = params
                 ,data = dtrain
                 ,nrounds = 500
                 ,nfold = 10L
                 ,feval = rsquare
                 ,early_stopping_rounds = 20
                 ,maximize = T
                 ,print_every_n = 10)

bst.train <- xgb.train(params = params, data = dtrain, nrounds = 162)

pred <- predict(bst.train, dtest)
pred <- exp(pred)-1


sub <- data.table(ID = test_id$ID, y = pred)
fwrite(sub,"sub1.csv") #0.14
fwrite(sub,"sub2.csv") #with ID feature #0.30193
fwrite(sub,"sub3.csv") #LB 0.53560 top 150
fwrite(sub,"sub4.csv") #0.54102 top 100
fwrite(sub,"sub5.csv") #0.54429 top 50





















