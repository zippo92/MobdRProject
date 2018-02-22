data_normalization3 <- function(my_data){
  data_train <- my_data$train[,1:(ncol(my_data$train))]
  
  data_test <- my_data$test[,1:(ncol(my_data$test)-1)]
  label_test <- my_data$test[,ncol(my_data$test)]
  
  data_train[,1:(ncol(my_data$train) - 1)] <- scale(data_train[, 1:(ncol(data_train))-1],center = T,scale = T)
  scaled_test <- scale(data_test,center = T,scale = T)
  
  output <- list(data_train,scaled_test,label_test)
  names(output) <- c("scaled_training","scaled_test","label_test")
  return(output)
}