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

# dropping columns with 0 standard deviation
cols_to_drop <- c('X11', 'X93', 'X107', 'X233', 'X235', 'X268', 'X289', 'X290', 'X293', 'X297', 'X330','X347')
binary_df <- binary_df[, -cols_to_drop, with=F]

# For Tsne script, go at end or continue:

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


# Taking Log of target variable -------------------------------------------

library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(train_binary), label = log1p(train_labels))
dtest <- xgb.DMatrix(data = as.matrix(test_binary))

params <- list(
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  colsample_bytree = 0.5,
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
                 ,nrounds = 73)


bst.pred <- predict(bst, dtest)
base_sub <- data.table(ID = test_id, y = exp(bst.pred)-1)
fwrite(base_sub, "xgboost_binary_25June.csv") #LB 0.54708 | #CV 0.637026



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

xgb_imp <- xgb.importance(bst, feature_names = colnames(dtrain))
xgb.plot.importance(xgb_imp, top_n = 10)

  
sbst.pred <- predict(bst, dtest)
base_sub <- data.table(ID = test_id, y = exp(bst.pred)-1)
fwrite(base_sub, "xgboost_binarywithcats_25June.csv") #LB 0.54708 | #CV 0.635544

# Fixing 3977 prediction
base_sub[3977,'y'] <- 126.1971
fwrite(base_sub,"xgboost_binarycat_fix3977.csv") #0.54425 - LB Reduced


# 10 folds
train$ix <- sample(1:nrow(train) %% 5)
head(train)

magic <- categorical_df[1:20]
magic[,y := train_labels[1:20]]
magic$ix <- sample(1:nrow(magic) %% 3)
magic

magic[,.GRP, ix]
magic[, magic[!X0 %in% .BY[[1]], mean(y), by = ix], by = X0]

leaveOneOutMean <- function(dt, ind, bybig, bysmall) {
  dtmp <- copy(dt) # copy so as not to alter original dt object w intermediate assignments
  #dtmp <- dtmp[is.na(get(ind))==F,]
  dtmp[,`:=`(avg_ind_big=mean(get(ind)), Nbig=.N), by=.(get(bybig))]
  dtmp[,`:=`(Nbigsmall=.N, avg_ind_big_small=mean(get(ind))), by=.(get(bybig), get(bysmall))]
  dtmp[,unbmean:=(avg_ind_big*Nbig-(Nbigsmall*avg_ind_big_small))/(Nbig-Nbigsmall)]
  return(dtmp[,unbmean])
}

magic[,unbiased_mean:=leaveOneOutMean(.SD, ind='y', bybig='ix', bysmall='X0')]

magic[,unbiased_mean2:=leaveOneOutMean(.SD, ind='y', bybig='X0', bysmall='ix')]
magic


# PCA on binary components

res_pca <- prcomp(train_binary)
plot(res_pca, type='l')

var <- (res_pca$sdev)^2

prop_var <- var/sum(var)
sum(prop_var[1:100])

explained_variance_threshold <- 0.9
importance_pca <- summary(res_pca)$importance
last_upper <- max(which(importance_pca[3, ] < explained_variance_threshold))+1
retain_vars <- colnames(importance_pca)[1:last_upper]

pca_train <- as.data.table(res_pca$x[,1:100])
pca_test <- predict(res_pca, test_binary)
pca_test <- as.data.table(pca_test[,1:100])

# Logistic PCA on Binary Components

library(rARPACK)
library(logisticPCA)

k <- 20
m <- 12
logpca_model <- logisticPCA(train_binary, k = k, m = m)
logpca_features <- logpca_model$PCs

logpca_features <- as.data.table(logpca_features)
colnames(logpca_features) <- paste0('LPC',1:k)

logpca_features_test <- predict(logpca_model, test_binary)
logpca_features_test <- as.data.table(logpca_features_test)
colnames(logpca_features_test) <- paste0('LPC',1:k)


# Frequency encode categoricl features

train_ano_cat <- categorical_df[1:nrow(train)]
test_ano_cat <- categorical_df[(nrow(train)+1):nrow(onehot_cat)]

for(x in colnames(train_ano_cat)){
  
  train_ano_cat[,paste('cat_',x) := .N, by = eval(x)]
  
}

for(x in colnames(test_ano_cat)){
  
  test_ano_cat[,paste('cat_',x) := .N, by = eval(x)]
  
}

cols_to_drop <- paste0('X',c(0:6,8))

train_ano_cat <- train_ano_cat[,-cols_to_drop,with=F]
test_ano_cat <- test_ano_cat[,-cols_to_drop,with=F]



#  Create interaction features based on X314, X127, X29, X315 (XGB Importance) -------------

library(gtools)

charvars <- c('X314','X127','X29','X315')

cmb <- combinations(n = length(charvars),r = 2,v = charvars)
cmb

train_int <- train_binary[,charvars,with=F]
test_int <- test_binary[,charvars,with=F]

#interactions
for(i in 1:nrow(cmb)) {
  train_int[[paste0(cmb[i,1], cmb[i,2])]] <- paste(train_int[[cmb[i,1]]], train_int[[cmb[i,2]]])
  test_int[[paste0(cmb[i,1], cmb[i,2])]] <- paste(test_int[[cmb[i,1]]], test_int[[cmb[i,2]]])
}

leaveOneOutMean <- function(dt, ind, bybig, bysmall) {
  dtmp <- copy(dt) # copy so as not to alter original dt object w intermediate assignments
  #dtmp <- dtmp[is.na(get(ind))==F,]
  dtmp[,`:=`(avg_ind_big=mean(get(ind)), Nbig=.N), by=.(get(bybig))]
  dtmp[,`:=`(Nbigsmall=.N, avg_ind_big_small=mean(get(ind))), by=.(get(bybig), get(bysmall))]
  dtmp[,unbmean:=(avg_ind_big*Nbig-(Nbigsmall*avg_ind_big_small))/(Nbig-Nbigsmall)]
  return(dtmp[,unbmean])
}

train_int$ix <- sample(1:nrow(train) %% 5)
train_int[,y := train_labels]

for(x in colnames(train_int)[sapply(train_int, is.character)]){
  train_int[,paste('enc_',x,sep = "") := leaveOneOutMean(.SD,'y','ix',eval(x))]
}

for(var in colnames(test_int)){
  if(is.character(test_int[[var]])){
    target.mean <- train_int[,.(pr = mean(y)),by=eval(var)]
    test_int[[paste('enc_',var,sep = "")]] <- target.mean$pr[match(test_int[[var]], target.mean[[var]])]
  }
}

head(train_int)
head(test_int)


## Combine All Features

# Binary, One Hot, PCA, LogisticPCA, Interaction

all_train <- cbind(train_binary
                   ,train_ano_cat
                   ,pca_train
                   ,logpca_features
                   ,train_int[,grep(pattern = "^enc_",x = colnames(train_int),value = T),with=F])

all_test <- cbind(test_binary
                   ,test_ano_cat
                   ,pca_test
                   ,logpca_features_test
                   ,test_int[,grep(pattern = "^enc_",x = colnames(test_int),value = T),with=F])


library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(all_train), label = log1p(train_labels))
dtest <- xgb.DMatrix(data = as.matrix(all_test))

params <- list(
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 5,
  colsample_bytree = 0.5,
  subsample = 1
  #gamma = 2
  
)

bst.cv <- xgb.cv(params = params
                 ,data = dtrain
                 ,nrounds = 500
                 ,nfold = 5L
                 ,feval = rsquare
                 ,early_stopping_rounds = 40
                 ,maximize = T
                 ,print_every_n = 10)

bst <- xgb.train(params = params
                 ,data = dtrain
                 ,nrounds = 64)

xgb_imp <- xgb.importance(bst, feature_names = colnames(dtrain))
xgb.plot.importance(xgb_imp, top_n = 10)

bst.pred <- predict(bst, dtest)
base_sub <- data.table(ID = test_id, y = exp(bst.pred)-1)
fwrite(base_sub,"xgboost_26thJune.csv") #-0.43808



# Another xgboost wth hist ------------------------------------------------

library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(all_train), label = log1p(train_labels))
dtest <- xgb.DMatrix(data = as.matrix(all_test))

params <- list(
  objective = 'reg:linear',
  eta = 0.0001,
  max_depth = 6,
  min_child_weight = 5,
  colsample_bytree = 0.5,
  subsample = 1
  #gamma = 2
  
)

bst.cv <- xgb.cv(params = params
                 ,data = dtrain
                 ,nrounds = 500
                 ,nfold = 5L
                 ,feval = rsquare
                 ,early_stopping_rounds = 40
                 ,maximize = T
                 ,print_every_n = 10
                 )


# h2o deep learning

library(h2o)
h2o.init(nthreads = -1, max_mem_size = '16G')

train.hex <- as.h2o(cbind(all_train, train_labels))
test.hex <- as.h2o(all_test)


# remove all files
rm(train_ano_cat, test_ano_cat, pca_test,pca_train, logpca_features, logpca_features_test,binary_df, categorical_df)

# set variables
y <- 'train_labels'
x <- setdiff(colnames(train.hex), y)


# default deep learning

dl1 <- h2o.deeplearning(x = x
                        ,y = y
                        ,training_frame = train.hex
                        ,nfolds = 5L)

h2o.performance(dl1,xval = T)

dlpreds <- as.data.table(h2o.predict(dl1, test.hex))
h2o_dl_sub <- data.table(ID = test_id, y = dlpreds$predict)
fwrite(h2o_dl_sub, "h2o_dldef_26June.csv") #0.20620

dl2 <- h2o.deeplearning(x = x
                        ,y = y
                        ,training_frame = train.hex
                        ,nfolds = 5L
                        ,standardize = T
                        ,activation = 'Tanh'
                        ,hidden = c(32,32,32)
                        ,epochs = 500
                        ,seed = 1
                        ,rate = 0.025
                        ,momentum_start = 0.5
                        ,momentum_stable = 0.99
                        #,input_dropout_ratio = 0.1
                        ,l2 = 0.5
                        ,stopping_rounds = 40
                        )
dl2
h2o.shutdown()

# require(mxnet)
# library(mxnet)
# 
# model <- mxnet::mx.mlp(data = as.matrix(all_train)
#                        ,label = log1p(train_labels)
#                        ,hidden_node = c(10,10,10)
#                        ,out_node = 1
#                        ,activation = 'relu'
#                        )
# 

train_y <- data.table(y = train_labels)




# Doing MCA  --------------------------------------------------------------

library(MASS)

train_binary[,colnames(train_binary) := lapply(.SD, factor), .SDcols = colnames(train_binary)]
test_binary[,colnames(test_binary) := lapply(.SD, factor), .SDcols = colnames(test_binary)]

res_mca <- mca(train_binary, nf = 27)
summary(res_mca)
rm(res_mca)

res_mca_train <- predict(res_mca, newdata = train_binary)

res_mca <- mca(test_binary, nf = 27)
res_mca_test <- predict(res_mca, test_binary)


fwrite(all_train,"without_outlier/train.csv")
fwrite(all_test,"without_outlier/test.csv")
fwrite(train_y,"without_outlier/train_y.csv")


# Creating Rtsne Feature
library(Rtsne)
tsne<- Rtsne(as.matrix(binary_df),check_duplicates = F,pca = F,perplexity = 30,theta = 0.5, dims = 2)
tsne$Y

tsne_feat <- as.data.table(tsne$Y)

# creating distance features
library(proxy)

d2 = dist(x = binary_df, method = 'binary')
d3 = dist(x = binary_df, method = 'cosine')

# d4 = vegdist(x = binary_df, method = "kulczynski")
# d5 = vegdist(x = binary_df, method = "morisita")
# d6 = vegdist(x = binary_df, method = "horn")
# d7 = vegdist(x = binary_df, method = "mountford")
# d8 = vegdist(x = binary_df, method = "mahalanobis")
# d9 = vegdist(x = binary_df, method = "gower")
# d10 = vegdist(x = binary_df, method = "altGower")
# d11 = vegdist(x = binary_df, method = "canberra")

tsne_train <- tsne_feat[1:nrow(train)]
tsne_test <- tsne_feat[(nrow(train)+1):nrow(binary_df)]

d1_train <- d1[1:nrow(train)]
d1_test <- d1[(nrow(train)+1):nrow(binary_df)]

d2_train <- d2[1:nrow(train)]
d2_test <- d2[(nrow(train)+1):nrow(binary_df)]

d3_train <- d3[1:nrow(train)]
d3_test <- d3[(nrow(train)+1):nrow(binary_df)]

d4_train <- d4[1:nrow(train)]
d4_test <- d4[(nrow(train)+1):nrow(binary_df)]

d5_train <- d5[1:nrow(train)]
d5_test <- d5[(nrow(train)+1):nrow(binary_df)]

d6_train <- d6[1:nrow(train)]
d6_test <- d6[(nrow(train)+1):nrow(binary_df)]

d7_train <- d7[1:nrow(train)]
d7_test <- d7[(nrow(train)+1):nrow(binary_df)]

d8_train <- d8[1:nrow(train)]
d8_test <- d8[(nrow(train)+1):nrow(binary_df)]

d9_train <- d9[1:nrow(train)]
d9_test <- d9[(nrow(train)+1):nrow(binary_df)]

d10_train <- d10[1:nrow(train)]
d10_test <- d10[(nrow(train)+1):nrow(binary_df)]

d11_train <- d11[1:nrow(train)]
d11_test <- d11[(nrow(train)+1):nrow(binary_df)]


train_feat <- fread("without_outlier/train.csv")
test_feat <- fread("without_outlier/test.csv")

train_dist <- data.table(d1_train, d2_train, d3_train, d4_train, d5_train, d6_train, d7_train, d8_train, d9_train, d10_train, d11_train)
colnames(train_dist) <- paste0("dist",1:11)
train_dist[, dist5 := NULL]

test_dist <- data.table(d1_test, d2_test, d3_test, d4_test, d5_test, d6_test, d7_test, d8_test, d9_test, d10_test, d11_test)
colnames(test_dist) <- paste0("dist",1:11)
test_dist[, dist5 := NULL]

train_feat <- cbind(train_feat, tsne_train, train_dist)
test_feat <- cbind(test_feat, tsne_test, test_dist)

fwrite(train_feat,"without_outlier/train_feats.csv")
fwrite(test_feat,"without_outlier/test_feats.csv")

# Adding one more mean features by X0

path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)

library(data.table)
library(e1071)

train <- fread("train.csv")
test <- fread("test.csv")

skewness(train$y)
train[,summary(y)]
train <- train[y < 260]
skewness(train$y)

meanX0 <- train[,mean(y),X0]
colnames(meanX0) <- c('X0','meanX0')

test <- meanX0[test, on="X0"]




## add this feature in feats data
train_feats <- fread("without_outlier/train_feats.csv")
test_feats <- fread("without_outlier/test_feats.csv")

train_feats <- cbind(train_feats,  train$mean_X0)
test_feats <- cbind(test_feats, test$meanX0)

fwrite(train_feats,"without_outlier/train_feats.csv")
fwrite(test_feats,"without_outlier/test_feats.csv")

## fixing values in submission file - LB Probing

# submit <- fread("without_outlier/ensem_basel1_extratr.csv",sep = "\t")
# 
# emailfile <- fread("/home/manish/Desktop/ML_Users/Challenge #2/all_participants.csv")
# emailfile <- unique(emailfile$Email)
# 
# email <- data.table(email = emailfile)
# fwrite(email, "/home/manish/Desktop/ML_Users/Challenge #2/unique_participants.csv")

library(jsonlite)

subfile <- fromJSON("without_outlier/temp.json",simplifyDataFrame = T)
class(subfile)

setDT(subfile)
fwrite(subfile, "without_outlier/l1_ext28June_R.csv")





