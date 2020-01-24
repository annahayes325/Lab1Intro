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
  covar=function(i,k){ #create a helper equation which holds the basic covariance function
    sum((i-mean(i))*(k-mean(k)))/length(i)
  }
  corlatn=function(i,k){
    covar(i,k)/(sqrt(covar(i,i))*sqrt(covar(k,k))) #create a helper equation which holds the basic correlation function
  }
  result=matrix(ncol(x)*ncol(x),nrow=ncol(x),ncol=ncol(x)) #create an empty matrix pxp matrix to hold the outputs
  for(col1 in 1:ncol(x)) { #use a double for loop to create a loop which will find the correlation between all sets of columns of x
    for(col2 in 1:ncol(x)) {
      i = x[, col1]
      k = x[, col2]

      result[col1,col2]=corlatn(i,k) #apply the correlation function to fill the empty matrix
    }
    colnames(result)=colnames(x) #add column names
    rownames(result)=colnames(x) #add row names
  }
  result #output the filled matrix
}
