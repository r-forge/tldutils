\name{sq_matrix}
\alias{sq_matrix}
\title{Takes a vector and converts it into a square matrix}
\usage{
  sq_matrix(data, byrow = TRUE)
}
\arguments{
  \item{data}{Object (probably vector) to be turned into
  square matrix}

  \item{byrow}{Whether to fill in the matrix by row or by
  column}
}
\value{
  A matrix with the same number of columns and rows
}
\description{
  \code{sq_matrix} takes a vector and converts it into a
  square matrix.
}
\examples{
sq_matrix(1:4)
sq_matrix(1:4, FALSE)
matrix(1:4, nrow=2)
}
\seealso{
  \code{link{matrix}}
}

