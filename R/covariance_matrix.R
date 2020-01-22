#' Covariance matrix function
#'
#' Takes a data array and returns the sample (biased) covariance matrix
#'
#' @param X
#'
#' @return a matrix (the sample (biased) covariance matrix of X)
#' @export
#'
#' @examples
#' X = [1 2 3
#'      4 5 6]; covariance_matrix(X)
#'
covariance_matrix = function(X) {

  s = matrix(0, nrow = ncol(X), ncol = ncol(X))

  for (i in 1:ncol(X))
    for (k in 1:ncol(X)) {
      for (j in 1:nrow(X))
        s[i,k] = s[i,k] + (X[j,i]-mean_vector(X)[i]) * (X[j,k]-mean_vector(X)[k])
        s[i,k] = s[i,k] / nrow(X)
    }
  return(s)
}
