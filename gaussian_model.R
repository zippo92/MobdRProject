gaussian_model <- function(TR, YTR){
  gaussian_model<-svm(x=TR,y=YTR,scale=F,type="C-classification",kernel="radial")
  return(gaussian_model)
}