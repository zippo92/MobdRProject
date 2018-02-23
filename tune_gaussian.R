tune_gaussian <- function(TR, YTR){
  tune_gauss <- tune.svm(TR, YTR, cost=10^(-1:1), gamma=10^(-1:1), kernel="radial");
  return(tune_gauss);
}