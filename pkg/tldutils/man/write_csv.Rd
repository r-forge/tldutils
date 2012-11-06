\name{write_csv}
\alias{write_csv}
\title{Creates a csv file (with different defaults to \code{utils::write.csv})
\code{write_csv} creates csv files with different defaults to \code{write.csv}, in particular
 by default we suppress row names and fill missing values with "" instead of "\code{NA}"}
\usage{
  write_csv(x, file, na = "", row.names = FALSE, ...)
}
\arguments{
  \item{x}{the object to be written passed to
  \code{write.csv}}

  \item{file}{passed to write.csv, use
  \code{file=gzfile(filename)} to write a gzip compressed
  file}

  \item{na}{String to use for missing values passed to
  \code{write.csv}}

  \item{row.names}{Passed to \code{write.csv}, determines
  whether to write row names or what they should be, passed
  to \code{write.csv}}

  \item{...}{Other arguments passed to \code{write.csv}}
}
\value{
  Returns \code{invisible(NULL)}, as a side effects creates
  a csv file.
}
\description{
  Creates a csv file (with different defaults to
  \code{utils::write.csv}) \code{write_csv} creates csv
  files with different defaults to \code{write.csv}, in
  particular by default we suppress row names and fill
  missing values with "" instead of "\code{NA}"
}
\seealso{
  \code{\link[utils]{write.csv}}
}

