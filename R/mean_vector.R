#' Mean vector function
#'
#'Takes a data array and returns the sample mean vector
#'
#' @param X
#'
#' @return a vector (the mean vector of X)
#' @export
#'
#' @examples
#' X = [1 2 3
#'      4 5 6]; mean_vector(X)
#'
mean_vector=function(X){
  y = rep (0 , ncol(X))

  for (i in 1:nrow(X))
    for (j in 1:ncol(X))
      y[j] = y[j] + X[i,j]

    for (j in 1:ncol(X))
      y[j] = y[j] / nrow(X)

    return(y)
}
