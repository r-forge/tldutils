\name{compute_sales_tax}
\alias{compute_sales_tax}
\title{Computes sales tax}
\usage{
  compute_sales_tax(pre_tax, tax_rate = 0.0825)
}
\arguments{
  \item{pre_tax}{The pre-tax price}

  \item{tax_rate}{The ad-valorem sales tax rate}
}
\value{
  Returns a list with pre_tax, sales_tax, post_tax, and
  tax_rate.
}
\description{
  \code{compute_sales_tax} computes sales tax and post tax
  figures
}

