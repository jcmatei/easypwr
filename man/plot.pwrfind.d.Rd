% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Solving.Plotting.R
\name{plot.pwrfind.d}
\alias{plot.pwrfind.d}
\title{Creates a data frame and plot of grouped observations for missing powers based on difference of means.}
\usage{
\method{plot}{pwrfind.d}(sdA0, sdA1, sdB0, sdB1, r1, alpha, m1, m2, n)
}
\arguments{
\item{sdA0}{A number.}

\item{sdA1}{A number.}

\item{sdB0}{A number.}

\item{sdB1}{A number.}

\item{r1}{A number.}

\item{alpha}{A number.}

\item{m1}{A number.}

\item{m2}{A number.}

\item{n}{A number.}
}
\description{
This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by mean difference and groups observations by similar non mean difference-inputs.
}
\examples{
plot.pwrfind.d(2,2,2,2,.5,.05,c(57,55,56),c(53,56,54),64)
}
