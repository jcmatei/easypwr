
\name{analyze.inputs}
\alias{analyze.inputs}
\title{Calculate and Plot Power-Related Variables}
\usage{
analyze.inputs(sdA0 = ,sdA1 = ,sdB0 = ,sdB1 = ,r1 = ,alpha =,m1 = ,m2 = ,power = , n = , ind = "")
}
\description{
Takes the above inputs with the exeption of (m1 & m1), power, or n, and uses them to calculate the omitted variable and plot it against a specified indicator variable (ind) either "diff", "power", or "sample".
}
\examples{
analyze.inputs(sdA0 = 2,sdA1 = 2,sdB0 = 2,sdB1 = 2,r1 = c(.5,.6,.7),alpha =.05,m1 = 55,m2 = 54,power = c(.6,.7,.8), ind = "power")

analyze.inputs(sdA0 = 2,sdA1 = 2,sdB0 = 2,sdB1 = 2,r1 = c(.5,.6,.7),alpha =.05, power = .7, n = c(60,64,68) ind = "sample")
}





