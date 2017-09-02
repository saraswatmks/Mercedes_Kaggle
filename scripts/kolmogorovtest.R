path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)

train <- fread("train.csv")
test <- fread("test.csv")

data = rbind(train, test, fill=T)

CompareSets <- function(test, train) {
  
  comprows <- ifelse(nrow(train) < nrow(test), nrow(train), nrow(test))
  fields <- intersect(names(train), names(test))
  fields <- setdiff(fields, 'Id')
  # fields <- setdiff(fields, names(which(sapply(train, class) =='factor')))
  
  tt_compare <- data.frame(NULL)
  for (name in sort(fields)) {
    if (class(train[[name]]) %in% c("numeric", "integer")) {
      plot(density(na.omit(train[1:comprows,name])), col=rgb(1,0,0,0.5), main=name)
      lines(density(na.omit(test[1:comprows,name])), col=rgb(0,0,1,0.5))
      tt_compare <- rbind(tt_compare, cbind(name, ks.test(train[,name], test[,name])$stati))
    } else if(length(unique(train[,name])) < 50 && class(train[[name]]) == 'factor') {
      plot(train[,name], col=rgb(1,0,0,0.5), main=name)
      par(new=TRUE)
      plot(test[,name], col=rgb(0,0,1,0.5))
    }
  }
  tt_compare$V2 <- as.numeric(as.character(tt_compare$V2))
  return(tt_compare)
}

features <- colnames(data)
for (f in features){
  if( (class(data[[f]]) == "character") || (class(data[[f]]) == "factor"))
  {
    levels = unique(data[[f]])
    data[[f]] = as.numeric(factor(data[[f]], level = levels)) 
  }
}

train = data[!is.na(y),] 
test = data[is.na(y),] 
test[,y:=NULL]

train = train[complete.cases(train),] 
test = test[complete.cases(test),] 

train = train[ ,.(X0, X1, X93)] %>% as.data.frame()
test  = test [ ,.(X0, X1, X93)] %>% as.data.frame()


par( mfcol=c(1,3),new=F )
tt_compare <- CompareSets(test, train)

#tt_compare <- CompareSets(test, train)
print("ks-test values between train and test: higher numbers are less similar:")
tt_compare[order(tt_compare$V2),]










