set.seed(123)
library("dplyr")
library("LiblineaR")
library("e1071")
source("data_understanding.R")
source("data_preparation.R")
source("data_splitting_nFold.R")
source("nFold_normalization.R")
source("nFold_normalization2.R")
source("data_normalization.R")
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
library("FSelector")

source("cross_validation.R")
source("gaussian_model.R")
source("gaussian_predict.R")


n_fold <- 5

train_set <- read.csv("training_R.csv",row.names=1, sep=";")

############ INSERIRE IL TEST SET QUI ############ 
# test_set <- ...;

data_summary <- data_understanding(train_set);
cleaned_train <- data_preparation(train_set);
cleaned_test <- data_preparation(test_set);

# classification_parameters <- cross_validation(cleaned_train, n_fold);
# View(classification_parameters);

normalized_data <- data_normalization(cleaned_train);
normalized_test <- data_normalization(cleaned_test);

best_model <- gaussian_model(normalized_data$scaled_data, normalized_data$label_data);
predict_model <- gaussian_predict(best_model, normalized_test$scaled_data);

confusion_matrix<-table(predicted=predict_model,observation=normalized_test$label_data);

accuracy <- round((confusion_matrix["0","0"]+confusion_matrix["1","1"])/
                    nrow(normalized_test$scaled_data),4)

TPR_0 <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)
TPR_1 <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)

kappa <- calculate_kappa(confusion_matrix);

output <- c(accuracy,TPR_0,TPR_1, kappa);
names(output) <- c("Accuracy", "AVG_TPR_0", "AVG_TPR_1", "Kappa");
View(t(output))






