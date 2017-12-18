new_feature_type1 <- function(data_trainm,train_label,data_testm,classes,p,corr,s){

  # feature map with laplacian
  train_nets <- structure(list(types = character(),
                               featureIDX = list(),
                               nets = list()))
  aa=1
  for(t in classes){
    class_train_data <- data_trainm[train_label==t,]
    nets <- network_build(class_train_data, p, corr)
    train_nets$types[[aa]] <- t
    train_nets$featureIDX[[aa]] <- colnames(data_trainm)
    train_nets$nets[[aa]] <- nets
    aa=aa+1
  }

  new_train = NULL
  new_test = NULL

  for(b in 1:length(train_nets$types)){
    nets <- train_nets$nets[[b]]

    r <- eigen(nets$laplacian)
    V <- r$vectors
    lam <- r$values
    lam[lam<0] = 0
    Lmbd = diag(lam ** abs(s))
    if(s<0){
      Lmbd = ginv(Lmbd)
    }
    newL = V %*% Lmbd %*% solve(V)
    new_train <- cbind(new_train,as.matrix(data_trainm) %*% newL)
    new_test <- cbind(new_test,as.matrix(data_testm) %*% newL)
  }

  new_data <- rbind(new_train,new_test)

  return(new_data)
}
