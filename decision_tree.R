decision_tree <- function(TR, TS, YTS){
  target <- shares~.
  tree <- rpart(target, data = TR, method = "class")
  prediction <- predict(tree, data.frame(TS), type = "class")
  confusion_matrix <- table(prediction, YTS)
  
  accuracy <- round((confusion_matrix["0","0"]+confusion_matrix["1","1"])/
                      nrow(TS),4)
  
  
  TPR_0 <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)
  TPR_1 <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)
  
  kappa <- calculate_kappa(confusion_matrix);
  return(c(accuracy,TPR_0,TPR_1, kappa))
}