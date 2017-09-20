# main function
network_classify <- function(L='label',data_train,data_test,nf,p,corr,f_type,s,nc,classifier,kern){

  nc = round(max(1,min(nc,nf/5)))
  newdata <- network_features(L='label',data_train,data_test,nf,p,corr,f_type,s,nc)

  # test
  if(classifier=="SVM")
  {
    require('e1071', quietly = TRUE)
    library(e1071)

    wts <- nrow(newdata$new_train) / table(newdata$train_label)
    model1 <- svm(newdata$new_train,newdata$train_label,type="C-classification",class.weights = wts,kernel = kern)
    prediction <- predict(model1, newdata$new_test)
  }

  if(classifier=="RF")
  {
    require('randomForest', quietly = TRUE)
    library(randomForest)
    model_rf <- randomForest(newdata$new_train,newdata$train_label,importance=TRUE, proximity=TRUE)
    prediction <- predict(model_rf, newdata$new_test)
  }

  return(list(pred = prediction, acc = sum(prediction==newdata$test_label)/nrow(data_test)))
}


