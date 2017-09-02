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

library(vegan)

one_hot <- model.matrix(~., data = categorical_df)
one_hot <- as.data.table(one_hot)

cat_vars <- nzv(one_hot)

one_hot <- one_hot[,-c(colnames(one_hot)[cat_vars]),with=F]

dist_metric <- c("manhattan", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao")

# dist_metric <- c("manhattan", "euclidean")
# 
# #distlist = vector('list', length(dist_metric))
# 
# for(x in seq(dist_metric)){
#   
#   distlist[[x]] <- vegdist(one_hot, method = dist_metric[[x]])
#   
# }


for(x in seq(dist_metric)){

  assign(paste0('dist',x), vegdist(x = one_hot, method = dist_metric[[x]]))

}

d1_train <- dist1[1:nrow(train)]
d1_test <- dist1[(nrow(train)+1):nrow(one_hot)]

d2_train <- dist2[1:nrow(train)]
d2_test <- dist2[(nrow(train)+1):nrow(one_hot)]

d3_train <- dist3[1:nrow(train)]
d3_test <- dist3[(nrow(train)+1):nrow(one_hot)]

d4_train <- dist4[1:nrow(train)]
d4_test <- dist4[(nrow(train)+1):nrow(one_hot)]

d5_train <- dist5[1:nrow(train)]
d5_test <- dist5[(nrow(train)+1):nrow(one_hot)]

d6_train <- dist6[1:nrow(train)]
d6_test <- dist6[(nrow(train)+1):nrow(one_hot)]

d7_train <- dist7[1:nrow(train)]
d7_test <- dist7[(nrow(train)+1):nrow(one_hot)]

d8_train <- dist8[1:nrow(train)]
d8_test <- dist8[(nrow(train)+1):nrow(one_hot)]

d9_train <- dist9[1:nrow(train)]
d9_test <- dist9[(nrow(train)+1):nrow(one_hot)]

d10_train <- dist10[1:nrow(train)]
d10_test <- dist10[(nrow(train)+1):nrow(one_hot)]

d11_train <- dist11[1:nrow(train)]
d11_test <- dist11[(nrow(train)+1):nrow(one_hot)]

d12_train <- dist12[1:nrow(train)]
d12_test <- dist12[(nrow(train)+1):nrow(one_hot)]

d13_train <- dist13[1:nrow(train)]
d13_test <- dist13[(nrow(train)+1):nrow(one_hot)]

d14_train <- dist14[1:nrow(train)]
d14_test <- dist14[(nrow(train)+1):nrow(one_hot)]

train_dist <- data.table(d1_train, d2_train, d3_train, d4_train, d5_train, d6_train, d7_train, d8_train, d9_train, d10_train, d11_train,d12_train,d13_train,d14_train)
colnames(train_dist) <- paste0("dist_cat",1:14)
train_dist[, dist_cat8 := NULL]

test_dist <- data.table(d1_test, d2_test, d3_test, d4_test, d5_test, d6_test, d7_test, d8_test, d9_test, d10_test, d11_test,d12_test,d13_test,d14_test)
colnames(test_dist) <- paste0("dist_cat",1:14)
test_dist[, dist_cat8 := NULL]

fwrite(train_dist,"features/dist_cat_train.csv")
fwrite(test_dist,"features/dist_cat_test.csv")



# Using Binary Data -------------------------------------------------------

head(binary_df)


# Remove NZV Features -----------------------------------------------------

def_feat <- nzv(binary_df)
binary_df <- binary_df[,-c(colnames(binary_df)[def_feat]),with=F]


for(x in seq(dist_metric)){
  
  assign(paste0('dist',x), vegdist(x = binary_df, method = dist_metric[[x]]))
  
}


d1_train <- dist1[1:nrow(train)]
d1_test <- dist1[(nrow(train)+1):nrow(binary_df)]

d2_train <- dist2[1:nrow(train)]
d2_test <- dist2[(nrow(train)+1):nrow(binary_df)]

d3_train <- dist3[1:nrow(train)]
d3_test <- dist3[(nrow(train)+1):nrow(binary_df)]

d4_train <- dist4[1:nrow(train)]
d4_test <- dist4[(nrow(train)+1):nrow(binary_df)]

d5_train <- dist5[1:nrow(train)]
d5_test <- dist5[(nrow(train)+1):nrow(binary_df)]

d6_train <- dist6[1:nrow(train)]
d6_test <- dist6[(nrow(train)+1):nrow(binary_df)]

d7_train <- dist7[1:nrow(train)]
d7_test <- dist7[(nrow(train)+1):nrow(binary_df)]

d8_train <- dist8[1:nrow(train)]
d8_test <- dist8[(nrow(train)+1):nrow(binary_df)]

d9_train <- dist9[1:nrow(train)]
d9_test <- dist9[(nrow(train)+1):nrow(binary_df)]

d10_train <- dist10[1:nrow(train)]
d10_test <- dist10[(nrow(train)+1):nrow(binary_df)]

d11_train <- dist11[1:nrow(train)]
d11_test <- dist11[(nrow(train)+1):nrow(binary_df)]

d12_train <- dist12[1:nrow(train)]
d12_test <- dist12[(nrow(train)+1):nrow(binary_df)]

d13_train <- dist13[1:nrow(train)]
d13_test <- dist13[(nrow(train)+1):nrow(binary_df)]

d14_train <- dist14[1:nrow(train)]
d14_test <- dist14[(nrow(train)+1):nrow(binary_df)]


train_dist <- data.table(d1_train, d2_train, d3_train, d4_train, d5_train, d6_train, d7_train, d8_train, d9_train, d10_train, d11_train,d12_train,d13_train,d14_train)
head(train_dist)

p1 <- nzv(train_dist)

train_dist <- train_dist[,-p1,with=F]
colnames(train_dist) <- paste0("dist_bin",1:12)

test_dist <- data.table(d1_test, d2_test, d3_test, d4_test, d5_test, d6_test, d7_test, d8_test, d9_test, d10_test, d11_test,d12_test,d13_test,d14_test)
test_dist <- test_dist[,-p1,with=F]
colnames(test_dist) <- paste0("dist_bin",1:12)

fwrite(train_dist,"features/dist_bin_train.csv")
fwrite(test_dist,"features/dist_bin_test.csv")


# all data ----------------------------------------------------------------

allvars <- cbind(one_hot, binary_df)

for(x in seq(dist_metric)){
  
  assign(paste0('dist',x), vegdist(x = allvars, method = dist_metric[[x]]))
  
}

d1_train <- dist1[1:nrow(train)]
d1_test <- dist1[(nrow(train)+1):nrow(allvars)]

d2_train <- dist2[1:nrow(train)]
d2_test <- dist2[(nrow(train)+1):nrow(allvars)]

d3_train <- dist3[1:nrow(train)]
d3_test <- dist3[(nrow(train)+1):nrow(allvars)]

d4_train <- dist4[1:nrow(train)]
d4_test <- dist4[(nrow(train)+1):nrow(allvars)]

d5_train <- dist5[1:nrow(train)]
d5_test <- dist5[(nrow(train)+1):nrow(allvars)]

d6_train <- dist6[1:nrow(train)]
d6_test <- dist6[(nrow(train)+1):nrow(allvars)]

d7_train <- dist7[1:nrow(train)]
d7_test <- dist7[(nrow(train)+1):nrow(allvars)]

d8_train <- dist8[1:nrow(train)]
d8_test <- dist8[(nrow(train)+1):nrow(allvars)]

d9_train <- dist9[1:nrow(train)]
d9_test <- dist9[(nrow(train)+1):nrow(allvars)]

d10_train <- dist10[1:nrow(train)]
d10_test <- dist10[(nrow(train)+1):nrow(allvars)]

d11_train <- dist11[1:nrow(train)]
d11_test <- dist11[(nrow(train)+1):nrow(allvars)]

d12_train <- dist12[1:nrow(train)]
d12_test <- dist12[(nrow(train)+1):nrow(allvars)]

d13_train <- dist13[1:nrow(train)]
d13_test <- dist13[(nrow(train)+1):nrow(allvars)]

d14_train <- dist14[1:nrow(train)]
d14_test <- dist14[(nrow(train)+1):nrow(allvars)]


train_dist <- data.table(d1_train, d2_train, d3_train, d4_train, d5_train, d6_train, d7_train, d8_train, d9_train, d10_train, d11_train,d12_train,d13_train,d14_train)
head(train_dist)

p1 <- nzv(train_dist)

train_dist <- train_dist[,-p1,with=F]
colnames(train_dist) <- paste0("dist_all",1:12)

test_dist <- data.table(d1_test, d2_test, d3_test, d4_test, d5_test, d6_test, d7_test, d8_test, d9_test, d10_test, d11_test,d12_test,d13_test,d14_test)
test_dist <- test_dist[,-p1,with=F]
colnames(test_dist) <- paste0("dist_all",1:12)

fwrite(train_dist,"features/dist_catbin_train.csv")
fwrite(test_dist,"features/dist_catbin_test.csv")








