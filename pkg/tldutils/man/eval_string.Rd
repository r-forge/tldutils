\name{eval_string}
\alias{eval_string}
\title{Evaluate strings in the current environment}
\usage{
  eval_string(string)
}
\arguments{
  \item{string}{A character vector containing legitimate R
  code}
}
\value{
  Returns \code{invisible(NULL)}.  As a side effect
  evaluates strings of R code in the environment that
  called \code{eval_string}.
}
\description{
  \code{eval_string} evaluates strings of R code in the
  current environment.
}
\examples{
eval("x <- 1") # x not assigned 1
eval(parse(text="y <- 2")) # y = 2
eval_string("z <- 3") # z = 3
}
\seealso{
  \code{\link{eval}}, \code{\link{parse}}
}

