set.seed(123)
library("dplyr")
library("LiblineaR")
library("e1071")
source("data_understanding.R")
source("data_preparation.R")
source("data_splitting_nFold.R")
source("data_normalization.R")
source("data_normalization2.R")
source("data_normalization3.R")
source("choosing_classifier2.R")
source("linear_SVM.R")
source("gaussian_SVM.R")
source("polinomial_SVM.R")
source("decision_tree.R")
source("calculate_kappa.R")
source("ada_boost.R")
library("rpart")
library("rpart.plot")
library("ada")

n_fold <- 5

fullSet <- read.csv("training_R.csv",row.names=1, sep=";")

data_summary <- data_understanding(fullSet);
cleaned_data <- data_preparation(fullSet);
#data_summary <- data_understanding(my_data);

splitted_data <- split(cleaned_data,n_fold);
#splitted_data contiene N split, ognuno diviso in train e test 
#Ex: splitted_data$split_2$train

normalized_splitted_data <- list();
normalized_splitted_data2 <- list();
normalized_splitted_data3 <- list();


# for(i in 1:n_fold){
#   normalized_splitted_data[[i]] <- data_normalization(splitted_data[[i]]);
#   names(normalized_splitted_data)[i] <-paste("split_",i, sep="");
# }
# 
# for(i in 1:n_fold){
#   normalized_splitted_data2[[i]] <- data_normalization2(splitted_data[[i]]);
#   names(normalized_splitted_data2)[i] <-paste("split_",i, sep="");
# }

for(i in 1:n_fold){
  normalized_splitted_data3[[i]] <- data_normalization3(splitted_data[[i]]);
  names(normalized_splitted_data3)[i] <-paste("split_",i, sep="");
}

classification_parameters = choosing_classifier(normalized_splitted_data3);
