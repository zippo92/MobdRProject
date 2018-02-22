choosing_classifier <- function(splits){
  set.seed(123);
  
  n_classifiers <- 1:1;
  n_fold <- length(splits);
  
  classification_parameters <- lapply(n_classifiers, function(x)matrix(0, nrow = n_fold, ncol = 4));
  names(classification_parameters) <- c("Boosting");

  for(i in 1:n_fold){
   
    # classification_parameters$Linear_SVM[i,] <- linear_SVM(splits[[i]]$scaled_training,
    #                                             splits[[i]]$label_train,
    #                                             splits[[i]]$scaled_test,
    #                                             splits[[i]]$label_test);
    
    # classification_parameters$Gaussian_SVM[i,] <- gaussian_SVM(splits[[i]]$scaled_training,
    #                                                        splits[[i]]$label_train,
    #                                                        splits[[i]]$scaled_test,
    #                                                        splits[[i]]$label_test);

    # classification_parameters$Polinomial_SVM[i,] <- polinomial_SVM(splits[[i]]$scaled_training,
    #                                                        splits[[i]]$label_train,
    #                                                        splits[[i]]$scaled_test,
    #                                                        splits[[i]]$label_test);

    # Decision Tree
    # classification_parameters$Decision_Tree[i,] <- decision_tree(splits[[i]]$scaled_training,
    #                                                              splits[[i]]$scaled_test,
    #                                                              splits[[i]]$label_test);
  
    # AdaBoosting
    classification_parameters$Boosting[i,] <- ada_boost(splits[[i]]$scaled_training,
                                                          splits[[i]]$scaled_test,
                                                          splits[[i]]$label_test)
    print(classification_parameters)
  }
  
  mean_parameter <- lapply(classification_parameters, function(x){
    x <- replace(x, is.na(x), 0)
    parameter_mean <- apply(x,2,mean)
    return(parameter_mean)
  })
  my_output <- do.call("rbind",mean_parameter)
  colnames(my_output) <- c("Avg_Accuracy","Avg_TPR0","Avg_TPR1", "Kappa")
  return(my_output)
}