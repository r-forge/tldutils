\name{write.dta.gz}
\alias{read.dta.gz}
\alias{write.dta.gz}
\title{Functions to read and write gzip compressed Stata files}
\usage{
  write.dta.gz(dataframe, file, ...)

  read.dta.gz(file, ...)
}
\arguments{
  \item{dataframe}{Dataframe}

  \item{file}{String giving filename}

  \item{...}{Passed onto either \code{write.dta} or
  \code{read.dta}}
}
\description{
  \code{read.dta.gz} and \code{write.dta.gz} allow one to
  read and write gzip compressed stata files
}
\examples{
df <- data.frame(x=rnorm(10), y=runif(10))
target <- tempfile("foo.dta.gz")
write.dta.gz(df, target)
df2 <- read.dta.gz(target)
all.equal(df, df2)
unlink(target)
}

