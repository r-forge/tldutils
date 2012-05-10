\name{knit_and_compile}
\alias{knit_and_compile}
\title{Knits and compiles a knitr compatible Sweave file}
\usage{
  knit_and_compile(file = NULL, pdf = TRUE, clean = TRUE,
    ...)
}
\arguments{
  \item{file}{A knitr compatible Sweave file}

  \item{pdf}{Logical, whether we should produce a pdf or
  dvi file}

  \item{clean}{Logical, whether we delete intermediate
  latex files}

  \item{...}{Passed onto tools::texi2dvi}
}
\description{
  \code{knit_and_compile} Knits and compiles a knitr
  compatible Sweave file
}
\seealso{
  \code{\link[knitr]{knit}, \link[tools]{texi2dvi}}
}

