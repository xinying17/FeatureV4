new_feature_type5 <- function(data_trainm,train_label,data_testm,classes,p,corr,s){

  # network classifier with single network
  train_nets <- structure(list(types = character(),
                               featureIDX = list(),
                               nets = list()))

  nets <- network_build(data_trainm, p, corr)
  train_nets$types <- c("all")
  train_nets$featureIDX <- colnames(data_trainm)
  train_nets$nets <- nets

  new_train = NULL
  new_test = NULL

  # nets <- train_nets$nets

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

  new_data <- rbind(new_train,new_test)

  return(new_data)
}
