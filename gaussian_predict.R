gaussian_predict <- function(model, TS){
  test_prediction <- predict(model, TS);
  return(test_prediction);
}