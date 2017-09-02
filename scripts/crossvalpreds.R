path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)

#### load
library(data.table)
library(e1071)

train <- fread("train.csv")
test <- fread("test.csv")

binda <- rbindlist(list(train, test), fill = T)

cols_to_conv <- setdiff(colnames(train),c('ID','y'))
binda[,(cols_to_conv) := lapply(.SD, function(x) as.numeric(as.factor(x))-1), .SDcols = cols_to_conv]

train <- binda[1:nrow(train)]
test <- binda[(nrow(train)+1):nrow(binda)]


# create fold structure
kf <- createFolds(train$y, k = 4,list = T)

maxL <- as.integer(max(sapply(kf, length)))

#initialize the fold frame
fold_ids <- data.frame(tmp = rep(-1, maxL))


for(i in 1:length(kf)){
  
  row.idx <- kf[[i]]
  
  ids <- train[row.idx]$ID
  if(length(ids) < maxL){
    num_has_needed <- maxL - length(ids)
    ids <- c(ids, rep(NA, num_has_needed))
  }
  
  fold_name <- paste0("Fold_",i)
  
  fold_ids[[fold_name]] <- ids
  
}
fold_ids$tmp <- NULL

# select names
feature.names <- setdiff(colnames(train),c('ID','y'))

# create meta containers
evalMatrix <- data.frame(Fold = numeric(), ID =numeric(), ground_truth = numeric(), xgb_master1 = numeric())
testMatrix <- data.frame(ID = test$ID)

cv <- c()

param <- list(objective = "reg:linear",
              max_depth = 9,
              eta = 0.01,
              subsample = 0.9,
              colsample_bytree = 0.5,
              min_child_weight = 1)

library(MLmetrics)
rsquare <- function(preds, dtrain){
  
  label <- getinfo(dtrain, 'label')
  err <- R2_Score(preds, label)
  return (list(metric = 'r2', value='err'))
  
}

start_time <- Sys.time()

for(i in 1:4){
  
  cat("\n---------------------------")
  cat("\n------- Fold: ", i, "----------")
  cat("\n---------------------------\n")
 
  idx <- fold_ids[[i]]
  idx <- idx[!is.na(idx)]
  
  trainingSet <- train[!train$ID %in% idx,]
  validationSet <- train[train$ID %in% idx,]
  
  cat("\nnrow train: ", nrow(trainingSet))
  cat("\nnrow eval: ", nrow(validationSet), "\n")
  
  dtrain <- xgb.DMatrix(data = as.matrix(trainingSet[,feature.names,with=F]), label = log1p(trainingSet$y))
  dval <- xgb.DMatrix(data = as.matrix(validationSet[,feature.names,with=F]), label = log1p(validationSet$y))
  
  watchlist <- list(OOB = dval, train = dtrain)
  
  clf <- xgb.train(params = param
                   ,data = dtrain
                   ,nrounds = 2130
                   ,print_every_n = 50
                   ,watchlist = watchlist
                   ,feval = rsquare)
  
  preds <- predict(clf, dval)
  df <- data.frame(Folds = rep(i, nrow(validationSet)), ID = validationSet$ID, ground_truth = validationSet$y, xgb_master1 = exp(preds)-1)
  eval_matrix <- rbind(evalMatrix, df)
  bestScore <- clf$bestScore
  bestIter <- clf$bestInd
  
  cv <- c(cv, bestScore)
  
  cat("\n*** fold score: ", bestScore)
  cat("\n*** best iter: ",bestIter)
  rm(clf, df, trainingSet, validationSet, watchlist)
  
  
  
}




