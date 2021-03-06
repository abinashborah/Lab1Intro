% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance_matrix.R
\name{covariance_matrix}
\alias{covariance_matrix}
\title{Covariance matrix function}
\usage{
covariance_matrix(X)
}
\arguments{
\item{X}{}
}
\value{
a matrix (the sample (biased) covariance matrix of X)
}
\description{
Takes a data array and returns the sample (biased) covariance matrix
}
\examples{
X = [1 2 3
     4 5 6]; covariance_matrix(X)

}
