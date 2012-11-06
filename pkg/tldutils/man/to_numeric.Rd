\name{to_numeric}
\alias{to_numeric}
\title{Takes out all the numbers in a string and converts to a number}
\usage{
  to_numeric(strings)
}
\arguments{
  \item{strings}{Character vector}
}
\value{
  Returns a vector of class "numeric".
}
\description{
  \code{to_string} extracts the numbers in a character
  vector and returns the extracted numbers as a numeric
  vector.
}
\examples{
strings <- c("FO23_43", "F10.4", "what3,00.", "what3..0..")
to_numeric(strings)
}

