data_normalization <- function(my_data){
  data_train <- my_data$train[,1:(ncol(my_data$train)-1)]
  label_train <- my_data$train[,ncol(my_data$train)]
  
  data_test <- my_data$test[,1:(ncol(my_data$test)-1)]
  label_test <- my_data$test[,ncol(my_data$test)]
  
  #scaled_training <- scale(data_train,center = T,scale = T)
  #toNormalize consiste nelle colonne da normalizzare, escludendo i binari e LDA
  toNormalize <- c(1:10,17:22,29:44);
  data_train[toNormalize] <- scale(data_train[toNormalize],center = T,scale = T)
  scaled_test <- scale(data_test,center = T,scale = T)
  
  output <- list(data_train,label_train,scaled_test,label_test)
  names(output) <- c("scaled_training","label_train","scaled_test","label_test")
  return(output)
}