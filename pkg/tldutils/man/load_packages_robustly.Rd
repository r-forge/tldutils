\name{load_packages_robustly}
\alias{load_packages_robustly}
\title{Loads packages, installing them if necessary}
\usage{
  load_packages_robustly(packages,
    repos = getOption("repos"))
}
\arguments{
  \item{packages}{Character vector of package names to be
  loaded}

  \item{repos}{Base URLs of repositories to use}
}
\value{
  Returns invisible(NULL), as side effect loads and
  possibly installs packages
}
\description{
  \code{load_packages_robustly} loads a character vector of
  packages installing any packages not currently installed
}

