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
  n=nrow(x) #determine the number of observations for each variable
  (colSums(x,dims=1))/n #sum the total observations for all variables and divide by n to get the average for each
}
