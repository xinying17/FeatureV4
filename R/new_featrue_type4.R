new_feature_type4 <- function(data_trainm,train_label,data_testm,classes,p,corr,s,nc){

  train_nets <- structure(list(types = character(),
                               featureIDX = list(),
                               nets = list()))

  # build network for each class
  aa = 1
  for(t in classes){
    class_train <- data_trainm[train_label==t,]
    clusters <- hclust(dist(t(as.matrix(class_train))),method = "ward.D")
    clusterCut <- cutree(clusters, nc)
    for(i in 1:nc){
      x = data.frame(class_train[,clusterCut==i])
      if(ncol(x)>2){
        nets <- network_build(as.matrix(x), p, corr)
        train_nets$types[[aa]] <- t
        train_nets$featureIDX[[aa]] <- colnames(x)
        train_nets$nets[[aa]] <- nets
        aa = aa+1
      }
    }

  }
  new_train <- matrix(nrow = nrow(data_trainm),ncol = length(train_nets$types))
  new_test <- matrix(nrow = nrow(data_testm),ncol = length(train_nets$types))

  # new train data
  for(b in 1:length(train_nets$types)){
    nets <- train_nets$nets[[b]]
    smooth_value <- smoothness(Lap = nets$laplacian,
                               data_trainm[,train_nets$featureIDX[[b]]],s)
    new_train[,b] <- smooth_value
  }

  # new test data
  for(b in 1:length(train_nets$types)){
    nets <- train_nets$nets[[b]]
    smooth_value <- smoothness(nets$laplacian,
                               data_testm[,train_nets$featureIDX[[b]]],s)
    new_test[,b] <- smooth_value
  }

  new_data1 <- rbind(new_train,new_test)
  new_data2 <- rbind(data_trainm,data_testm)

  new_data <- cbind(new_data2,new_data1)

  return(new_data)
}
