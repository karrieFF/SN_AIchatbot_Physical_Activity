% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/innovators.R
\name{determine_adopter}
\alias{determine_adopter}
\title{The title of -get innovators-}
\usage{
determine_adopter(
  total_n,
  adj_matrix,
  final_matrix,
  indices,
  p_innovators,
  method
)
}
\arguments{
\item{final_matrix}{A Numeric Matrix.}

\item{p_innovators}{A Numeric scalar.}

\item{method}{A string.}
}
\value{
A Numeric Vector \code{doipkg_innovators}:
\item{final_matrix}{Numeric Matrix.}
\item{p_innovators}{Numeric Vector.}
\item{final_matrixp_innovators}{Numeric scalar. the calculation of  \code{final_matrix} and \code{p_innovators}}
}
\description{
Here is a brief description
}
\details{
Get the innovators of \code{final_matrix} and \code{p_innovators}.
}
