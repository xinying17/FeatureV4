network_features <- function(L='label',data_train,data_test,nf,p,corr,f_type,s,nc,norm)
{
  classes <- unique(data_train$label)

  names(data_train)[colnames(data_train)==L] <- paste("label")
  names(data_test)[colnames(data_test)==L] <- paste("label")

  data_trainm <- data_train[,colnames(data_train)!=L]
  data_testm <- data_test[,colnames(data_test)!=L]


  train_label <- data_train$label
  test_label <- data_test$label

  # feature selection
  if(nf < ncol(data_trainm)) {
    nf = round(min(ncol(data_train),nf))

    # rank feature by ttest
    indx <- rankfeature(L,data_train,classes,nf)
    train_label <- data_train[,colnames(data_train)==L]
    data_trainm <- data_trainm[,indx]
    test_label <- data_test[,colnames(data_test)==L]
    data_testm <- data_testm[,indx]
  }


  # feature map
  if(f_type==1){
    new_data <- new_feature_type1(data_trainm,train_label,data_testm,classes,p,corr,s)
  }

  # network classifier with 2 networks
  if(f_type==2){
    new_data <- new_feature_type2(data_trainm,train_label,data_testm,classes,p,corr,s,norm)
  }

  # network classifier with 2*nc networks
  if(f_type==3){
    new_data <- new_feature_type3(data_trainm,train_label,data_testm,classes,p,corr,s,nc,norm)
  }

  # add new features to original data
  if(f_type==4){
    new_data <- new_feature_type4(data_trainm,train_label,data_testm,classes,p,corr,s,nc)
  }

  # network classifier with single network
  if(f_type==5){
    new_data <- new_feature_type5(data_trainm,train_label,data_testm,classes,p,corr,s)
  }
  # remove na and inf
  new_data <- data.frame(scale(new_data))

  is.na(new_data) <- sapply(new_data, is.infinite)
  is.na(new_data) <- sapply(new_data, is.nan)
  ind_na <- colSums(is.na(new_data))==0
  new_data <- new_data[,ind_na]


  new_train <- new_data[xx,]
  new_test <- new_data[-xx,]

  return(list(new_train = new_train, new_test = new_test, train_label = train_label, test_label = test_label))

}
