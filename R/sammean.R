#' Sample Mean Vector
#'
#' Takes a matrix and outputs sample means for all columns in vector form
#'
#' @param x a matrix
#'
#' @return A vector of sample means of all variables
#' @export
#'
#' @examples sammean(x)
sammean=function(x){
  n=nrow(x)
  (colSums(x,dims=1))/n
}
