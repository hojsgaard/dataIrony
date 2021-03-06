% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataIrony-ES.R
\name{es_fit}
\alias{es_fit}
\title{Fit exponential smoothing object.}
\usage{
es_fit(object, n.ahead = 1)
}
\arguments{
\item{object}{Exponential smoothing object}

\item{n.ahead}{Number of steps ahead used in fitting.}
}
\description{
Fit exponential smoothing object by minimizing the squared \code{n.ahead} steps prediction error.
}
