set.seed(123)
library("dplyr")
library("LiblineaR")
library("e1071")
source("data_understanding.R")
source("data_preparation.R")
source("data_splitting_nFold.R")
source("data_normalization.R")

fullSet <- read.csv("training_R.csv",row.names=1, sep=";")

data_summary <- data_understanding(fullSet);
cleaned_data <- data_preparation(fullSet);
#data_summary <- data_understanding(my_data);

splitted_data <- split(cleaned_data,5);
#splitted_data contiene N split, ognuno diviso in train e test 
#Ex: splitted_data$split_2$train
