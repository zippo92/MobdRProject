data_normalization3 <- function(my_data){
  data_train <- my_data$train[,1:(ncol(my_data$train))]
  
  data_test <- my_data$test[,1:(ncol(my_data$test)-1)]
  label_test <- my_data$test[,ncol(my_data$test)]
  
  #scaled_training <- scale(data_train,center = T,scale = T)
  #toNormalize consiste nelle colonne da normalizzare, escludendo i binari e LDA
  toNormalize <- c(1:9,16:27,29:47);
  data_train[toNormalize] <- scale(data_train[toNormalize],center = T,scale = T)
  data_test[toNormalize] <- scale(data_test[toNormalize],center = T,scale = T)
  
  output <- list(data_train,data_test,label_test)
  names(output) <- c("scaled_training","scaled_test","label_test")
  return(output)
}