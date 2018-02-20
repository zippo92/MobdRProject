calculate_kappa <- function(confusion_matrix){
  set.seed(123);
  
  TS <- confusion_matrix["0", "0"] + confusion_matrix["0", "1"] +
    confusion_matrix["1", "0"] + confusion_matrix["1", "1"]
  
  acc <- round((confusion_matrix["0","0"]+confusion_matrix["1","1"])/
                      TS,4)
  aexp <- round((confusion_matrix["0", "0"] + confusion_matrix["1", "0"]) * 
             (confusion_matrix["0", "0"] + confusion_matrix["0", "1"]) /
             TS, 4)
  dexp <- round((confusion_matrix["1", "1"] + confusion_matrix["1", "0"]) * 
                  (confusion_matrix["1", "1"] + confusion_matrix["0", "1"]) /
                  TS, 4)
  Pexp <- round((aexp + dexp) / TS, 4)
  kappa <- round(((acc - Pexp) / (1 - Pexp)), 4);
  return(kappa);
}