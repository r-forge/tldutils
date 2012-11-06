\name{mkdir}
\alias{mkdir}
\title{Creates directories (suppressing warnings and creates parents by default)
\code{mkdir} Takes a character vector of directories and creates them.}
\usage{
  mkdir(directory, parents = TRUE, mode = "0777")
}
\arguments{
  \item{directory}{Character vector of directories to
  create}

  \item{parents}{If \code{TRUE} (the default) create parent
  directories if they do not exist}

  \item{mode}{mode to be used on Unix-alikes}
}
\value{
  Returns invisibly a logical vector indicating if the
  operation succeeded for each directory attempted, as a
  side effect creates directories.
}
\description{
  Creates directories (suppressing warnings and creates
  parents by default) \code{mkdir} Takes a character vector
  of directories and creates them.
}
\seealso{
  \code{\link[base]{dir.create}}
}

