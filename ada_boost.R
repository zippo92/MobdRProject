ada_boost <- function(TR, TS, YTS){

  ada_model<-ada(shares~. ,data=TR,iter=70,nu=1,type="discrete")
  ##add testing data set
  test_prediction <- predict(ada_model, data.frame(TS), decisionValues = TRUE)
  #matrice di confusione sui dati di test
  confusion_matrix<-table(predicted=test_prediction,
                          observation=YTS)
  #nella matrice di confusione:
  #elemento [i,j] classe predetta i classe reale j
  accuracy <- round((confusion_matrix["0","0"]+confusion_matrix["1","1"])/
                      nrow(TS),4)
  
  
  TPR_0 <- round(((confusion_matrix["0","0"])/(confusion_matrix["0","0"]+confusion_matrix["1","0"])),4)
  TPR_1 <- round(((confusion_matrix["1","1"])/(confusion_matrix["1","1"]+confusion_matrix["0","1"])),4)
  
  kappa <- calculate_kappa(confusion_matrix);
  return(c(accuracy,TPR_0,TPR_1, kappa))
}