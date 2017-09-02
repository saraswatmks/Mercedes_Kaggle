path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)

library(xgboost)
library(data.table)

submission <- TRUE

data_train <- fread("train.csv")
data_test <- fread("test.csv")

dat

data_train[y > 170]$y <- 170

#concatenate train and test
data_1 <-  rbind(data_train[, c(1,3:ncol(data_train),2), with=FALSE], cbind(data_test, y=NA))

#new data
data_2 <- data_1[,.(ID, y)]

#X0 - X8 one hot encoding
for(i in 2:9){
  var <- colnames(data_1)[i]
  values <- unique(data_1[[var]])
  for(j in 1:length(values)){
    data_2[[paste0(var,"=",values[j])]] <- (data_1[[var]] == values[j]) * 1
  }
}

# X10 - X385: invert if the number 0f 0 is larger than that of 1

for(i in 10:ncol(data_1)-1){
  var <- colnames(data_1)[i]
  if(sum(data_1[[var]] == 1) > 4209){
    data_2[[paste0(var,"=0")]] <- 1 - data_1[[var]]
  } else {
    data_2[[paste0(var,"=1")]] <- data_1[[var]]
  }
  
}

# omit duplicated variables

dup_vars <- colnames(data_2)[which(duplicated(t(data_2)))]
data_2 <- data_2[,setdiff(colnames(data_2), dup_vars),with=F]

# calculate standard deviation of y by each variable

stats <- data.table()
for(var in setdiff(colnames(data_2),c('ID','y'))){
  temp <- data_2[!is.na(y), .(N = .N, sd=sd(y)), by=eval(var)]
  colnames(temp)[1] <- 'value'
  temp$var <- var
  #readline("Enter a number: %s")
  stats <- rbind(stats, temp[,c(4,1,2,3),with=F])
}

# calculate p-value of F-test for each variable
stats$p_value <- NA
for(i in 1:nrow(stats)){
  
  stats$p_value[i] <- pf((sd(data_train$y)/stats$sd[i])^2, stats$N[i]-1, nrow(data_train)-1, lower.tail = F)
  
}
stats <- stats[order(p_value, -N)]
stats <- stats[!duplicated(stats$var) | stats$var %in% paste0("X",0:9)]

# select significant variables as explanatory variables
exp.vars <- stats[N > 50 & log(p_value) < -10]$var
exp.vars
data_3 <- data_2[,c('ID','y', exp.vars),with=F]

# Modeling

data_train <- data_3[!is.na(y)]
data_test <- data_3[is.na(y)]

set.seed(0)
ids <- sample(nrow(data_train))

submission <- T
n.folds <- ifelse(submission,1,5)

score <- data.table()
result <- data.table()


for(i in 1:n.folds){
  
  if(submission){
    x.train <- data_train[,!"y",with=FALSE]
    x.test <- data_test[,!"y",with=FALSE]
    y.train <- data_train$y
  } else {
    
    train.ids <- ids[-seq(i,length(ids),by=n.folds)]
    test.ids <- ids[seq(i, length(ids),by=n.folds)]
    
    x.train <- data_train[train.ids, !"y",with=F]
    x.test <- data_train[test.ids, !"y",with=F]
    y.train <- data_train$y[train.ids]
    y.test <- data_train$y[test.ids]
    
    
  }
  
  x.train <- apply(x.train, 2, as.numeric)
  x.test <- apply(x.test,2,as.numeric)
  
  if(submission){
    
    nrounds <- 150
    early_stopping_rounds <- NULL
    dtrain <- xgb.DMatrix(data = as.matrix(x.train), label=y.train)
    dtest <- xgb.DMatrix(data = as.matrix(x.test))
    watchlist <- list(train=dtrain)
  
  } else {
    nrounds <- 3000
    early_stopping_round <- 100
    dtrain <- xgb.DMatrix(data = as.matrix(x.train), label=y.train)
    dtest <- xgb.DMatrix(data = as.matrix(x.test), label=y.test)
    watchlist <- list(train = dtrain, test = dtest)
  }
  
  params <- list(
    eta = 0.05,
    max_depth = 3,
    objective = 'reg:linear',
    eval_metric = 'rmse',
    base_score = mean(y.train)
  )
  
  model.xgb <- xgb.train(params = params
                         ,data = dtrain
                         ,nrounds = nrounds
                         ,watchlist = watchlist
                         ,early_stopping_rounds = early_stopping_rounds
                         ,print_every_n = 50)
  
  pred <- predict(model.xgb, dtest, reshape = T)
  
  if(submission) {
    result <- cbind(data_test[, .(ID)], y=pred)
  } else {
    score <- rbind(score, data.table(r2=1-model.xgb$best_score^2/var(y.test), best_score=model.xgb$best_score, best_iteration=model.xgb$best_iteration))
    temp <- cbind(data_train[test.ids, .(ID, y)], pred=pred)
    result <- rbind(result, temp)
  }
  
}

if(submission) {
  write.csv(result, "submission.csv", row.names=FALSE)
} else {
  print(mean(score$best_score))
  print(score$r2)
  print(mean(score$r2))
  print(1-mean((result$y-result$pred)^2)/var(result$y))
}

fwrite(result,"jack_solution.csv")












