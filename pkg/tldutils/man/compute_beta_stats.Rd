\name{compute_beta_stats}
\alias{compute_beta_stats}
\title{Prints various stats of a beta distribution}
\usage{
  compute_beta_stats(alpha, beta)
}
\arguments{
  \item{alpha}{Numeric value for alpha parameter of beta
  distribution}

  \item{beta}{Numeric value for beta parameter of beta
  distribution}
}
\value{
  Returns a list containing the alpha parameter value, beta
  parameter value, mean, and variance
}
\description{
  \code{compute_beta_stats} calculates mean and variance of
  beta distribution
}
\examples{
compute_beta_stats(1, 2)
compute_beta_stats(2, 1)
}

