data_normalization <- function(my_data){
  data <- my_data[,1:(ncol(my_data)-1)]
  label <- my_data[,ncol(my_data)]
  
  #scaled_training <- scale(data_train,center = T,scale = T)
  #toNormalize consiste nelle colonne da normalizzare, escludendo i binari e LDA
  toNormalize <- c(1:9,16:27,29:47);
  data[toNormalize] <- scale(data[toNormalize],center = T,scale = T)

  output <- list(data,label)
  names(output) <- c("scaled_data","label_data")
  return(output)
}