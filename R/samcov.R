#' Sample Covariance of Vectors
#'
#' Takes a matrix and outputs the sample covariance matrix of the variables represented.
#'
#' @param x a matrix
#'
#' @return a covariance matrix
#' @export
#'
#' @examples samcov(paper.df)
samcov=function(x){
  covar=function(i,k){
    sum((i-mean(i))*(k-mean(k)))/length(i)
  }
  result=matrix(ncol(x)*ncol(x),nrow=ncol(x),ncol=ncol(x))
  for(col1 in 1:ncol(x)) {
    for(col2 in 1:ncol(x)) {
      i = x[, col1]
      k = x[, col2]

      result[col1,col2]=covar(i,k)
    }
  }
  result
}
