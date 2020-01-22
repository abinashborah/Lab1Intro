#' Correlation matrix function
#'
#' Takes a data array and returns the sample correlation matrix
#'
#' @param X
#'
#' @return a matrix (the sample correlation matrix of X)
#' @export
#'
#' @examples
#' X = [1 2 3
#'      4 5 6]; correlation_matrix(X)
#'
correlation_matrix = function(X) {

  r = matrix(0, nrow = ncol(X), ncol = ncol(X))

  for (i in 1:ncol(X))
    for (k in 1:ncol(X)) {
      s1 = 0;
      s2 = 0;
      for (j in 1:nrow(X)) {
        r[i,k] = r[i,k] + (X[j,i]-mean_vector(X)[i]) * (X[j,k]-mean_vector(X)[k])
        s1 = s1 + (X[j,i]-mean_vector(X)[i]) * (X[j,i]-mean_vector(X)[i])
        s2 = s2 + (X[j,k]-mean_vector(X)[k]) * (X[j,k]-mean_vector(X)[k])
      }
      s1 = sqrt(s1)
      s2 = sqrt(s2)
      r[i,k] = r[i,k] / (s1*s2)
    }
  return(r)
}
