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
#binary_df <- df_all[,-categorical_vars, with=F]

head(categorical_df)
library(gtools)

char.vars <- colnames(categorical_df)

# 2 way combinations
cmb <- combinations(n = length(char.vars),r = 2,v = char.vars)


for(i in 1:nrow(cmb)){
  
  categorical_df[[paste0(cmb[i,1], cmb[i,2])]] <- paste0(categorical_df[[cmb[i,1]]], categorical_df[[cmb[i,2]]])
  
}

# 3 way interaction
cmb <- combinations(n = length(char.vars), r = 3, v = cmb)

for(i in 1:nrow(cmb)){
  
  categorical_df[[paste0(cmb[i,1], cmb[i,2], cmb[i,3])]] <- paste0(categorical_df[[cmb[i,1]]], categorical_df[[cmb[i,2]]], categorical_df[[cmb[i,3]]])
  
}

#4 way interaction
cmb <- combinations(n = length(char.vars), r = 4, v = char.vars)

for(i in 1:nrow(cmb)){
  
  categorical_df[[paste0(cmb[i,1], cmb[i,2], cmb[i,3], cmb[i,4])]] <- paste0(categorical_df[[cmb[i,1]]], categorical_df[[cmb[i,2]]], categorical_df[[cmb[i,3]]], categorical_df[[cmb[i,4]]])
  
}

#5 way interaction
cmb <- combinations(n = length(char.vars), r = 5, v = char.vars)

for(i in 1:nrow(cmb)){
  
  categorical_df[[paste0(cmb[i,1], cmb[i,2], cmb[i,3], cmb[i,4], cmb[i,5])]] <- paste0(categorical_df[[cmb[i,1]]], categorical_df[[cmb[i,2]]], categorical_df[[cmb[i,3]]], categorical_df[[cmb[i,4]]], categorical_df[[cmb[i,5]]])
  
}

library(stringr)
two_way <- c()

for(x in colnames(categorical_df)){
  k = nchar(x)
  if (k != 4 ) next 
  else ( two_way <- append(two_way, x))
}

three_way <- c()

for(x in colnames(categorical_df)){
  k = nchar(x)
  if(k != 6) next
  else ( three_way <- append(three_way, x))
}

four_way <- c()
five_way <- c()

for(x in colnames(categorical_df)){
  k = nchar(x)
  if(k == 8) four_way <- append(four_way, x)
  if(k == 10) five_way <- append(five_way, x)
}

two_way <- categorical_df[,two_way, with=F]
three_way <- categorical_df[,three_way,with=F]
four_way <- categorical_df[,four_way,with=F]
five_way <- categorical_df[,five_way,with=F]

# integer encoding

two_way[,(colnames(two_way)) := lapply(.SD, function(x) as.integer(as.factor(x))-1)]
three_way[,(colnames(three_way)) := lapply(.SD, function(x) as.integer(as.factor(x))-1)]
four_way[,(colnames(four_way)) := lapply(.SD, function(x) as.integer(as.factor(x))-1)]
five_way[,(colnames(five_way)) := lapply(.SD, function(x) as.integer(as.factor(x))-1)]

train_two <- two_way[1:nrow(train)]
test_two <- two_way[(nrow(train)+1):nrow(two_way)]

train_three <- three_way[1:nrow(train)]
test_three <- three_way[(nrow(train)+1):nrow(three_way)]

train_four <- four_way[1:nrow(train)]
test_four <- four_way[(nrow(train)+1):nrow(four_way)]

train_five <- five_way[1:nrow(train)]
test_five <- five_way[(nrow(train)+1):nrow(five_way)]

fwrite(train_two,"features/two_way_train.csv")
fwrite(test_two,"features/two_way_test.csv")

fwrite(train_three,"features/three_way_train.csv")
fwrite(test_three,"features/three_way_test.csv")

fwrite(train_four,"features/four_way_train.csv")
fwrite(test_four,"features/four_way_test.csv")

fwrite(train_five,"features/five_way_train.csv")
fwrite(test_five,"features/five_way_test.csv")









