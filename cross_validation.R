cross_validation <- function(cleaned_data, n_fold){
  splitted_data <- split(cleaned_data,n_fold);
  #splitted_data contiene N split, ognuno diviso in train e test 
  #Ex: splitted_data$split_2$train
  
  normalized_splitted_data <- list();
  normalized_splitted_data2 <- list();
  
  for(i in 1:n_fold){
    normalized_splitted_data[[i]] <- nFold_normalization(splitted_data[[i]]);
    names(normalized_splitted_data)[i] <-paste("split_",i, sep="");
  }
  
  for(i in 1:n_fold){
    normalized_splitted_data2[[i]] <- nFold_normalization2(splitted_data[[i]]);
    names(normalized_splitted_data2)[i] <-paste("split_",i, sep="");
  }
  
  classification_parameters = choosing_classifier(normalized_splitted_data, normalized_splitted_data2);
  return(classification_parameters);
}