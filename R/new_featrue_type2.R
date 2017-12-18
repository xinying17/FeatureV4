new_feature_type2 <- function(data_trainm,train_label,data_testm,classes,p,corr,s,norm){

  # network classifier with 2 networks
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

  data_matrixx <- rbind(data_trainm,data_testm)
  data_matrixx <- data.frame(scale(data_matrixx))
  xx = seq(from=1,to=nrow(data_trainm),by=1)

  if(norm==1){
    data_trainm <- data_matrixx[xx,]
    data_testm <- data_matrixx[-xx,]
  }

  for(b in 1:length(train_nets$types)){
    nets <- train_nets$nets[[b]]

    r <- eigen(nets$laplacian)
    V <- r$vectors
    lam <- r$values
    lam[lam<0] = 0
    Lmbd = diag(lam ** abs(s))
    newL = V %*% Lmbd %*% solve(V)
    lap_fun <- function(x) {x %*% newL %*% x}
    new_train <- cbind(new_train,apply(as.matrix(data_trainm),1,lap_fun))
    new_test <- cbind(new_test,apply(as.matrix(data_testm),1,lap_fun))
  }

  new_data <- rbind(new_train,new_test)

  return(new_data)
}
