#' Sample Correlation of Vectors
#'
#' Takes a matrix and outputs the sample correlation matrix of the variables represented
#'
#' @param x a matrix
#'
#' @return a correlation matrix
#' @export
#'
#' @examples samcor(paper.df)
samcor=function(x){
  covar=function(i,k){
    sum((i-mean(i))*(k-mean(k)))/length(i)
  }
  corlatn=function(i,k){
    covar(i,k)/(sqrt(covar(i,i))*sqrt(covar(k,k)))
  }
  result=matrix(ncol(x)*ncol(x),nrow=ncol(x),ncol=ncol(x))
  for(col1 in 1:ncol(x)) {
    for(col2 in 1:ncol(x)) {
      i = x[, col1]
      k = x[, col2]

      result[col1,col2]=corlatn(i,k)
    }
  }
  result
}
