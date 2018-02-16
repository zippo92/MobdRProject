source("replace_outlier.R");

data_preparation <- function(my_data){
  
  set.seed(123);
  # source("replace_outliers.R")
  
  # Rimozione dei duplicati
  my_data <- my_data[!duplicated(my_data),];
  
  # Rimozione articoli con zero parole
  my_data <- my_data[!(my_data$n_tokens_content == 0),];
  
  # Trasformiamo lo share in binario
  my_data <- share_binary(my_data);
  
  # Eliminiamo le colonne giornaliere, lasciando solo il weekend come binaria
  my_data <- delete_days(my_data);
  
  # Eliminiamo le non-stop words (uniche e non) in quanto non significative
  my_data <- delete_non_stop(my_data);
  
  # Eliminiamo le keyword massime e minime (shares) e teniamo solo le medie
  my_data <- delete_min_max_kw(my_data);
  
  # Replace outliers
  my_data <- search_and_replace_outlier(my_data);
  
  return(my_data);
}


share_binary <- function(my_data){
  my_data$shares <- ifelse(my_data$shares > 1400, 1, 0);
  return(my_data);
}

delete_days <- function(my_data){
  my_data <- my_data[, -c(31:37)];
  return(my_data);
}

delete_non_stop <- function(my_data){
  my_data <- my_data[, -c(5,6)];
  return(my_data);
}

delete_min_max_kw <- function(my_data){
  my_data <- my_data[, -c(17,18,20,21,23,24)];
  return(my_data);
}

search_and_replace_outlier <- function(my_data){
  my_data$n_tokens_title <- replace_outlier(my_data$n_tokens_title);
  my_data$n_tokens_content <- replace_outlier(my_data$n_tokens_content);
  my_data$num_imgs <- replace_outlier(my_data$num_imgs);
  my_data$num_hrefs <- replace_outlier(my_data$num_hrefs);
  my_data$num_videos <- replace_outlier(my_data$num_videos);
  
  return(my_data);
}
