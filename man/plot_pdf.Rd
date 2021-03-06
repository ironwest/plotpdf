% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotpdfs.R
\name{plot_pdf}
\alias{plot_pdf}
\title{Plot PDF by ggplot.}
\usage{
plot_pdf(distribution, xrange, ...)
}
\arguments{
\item{distribution}{Character string indicating probability distribution function. Eg "norm", "t", "chisq". Internally, "d", "p" are added as prefix of string. eg. dnorm(), pnorm().}

\item{xrange}{X axis range for plot.}

\item{...}{Arguments passed on to distribution. eg mean=1, sd = 2 for "norm".}
}
\description{
This function will plot PDF of any built in function.
}
