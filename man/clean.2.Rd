% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Output.clean.R
\name{clean.2}
\alias{clean.2}
\title{Further cleans the list which is the output of analyze.input}
\usage{
clean.2(obj)
}
\arguments{
\item{obj}{A list.}
}
\value{
A list oflength 1.
}
\description{
This function takes a two element list with one NULL element and will either eliminate the first element if it is NULL or eliminate the last one if it is not.
}
\examples{
clean.2(obj)
}
