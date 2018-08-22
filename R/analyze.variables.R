#'A function which will execute ind.sample function if the function is missing the sample size variable.
#'
#'This function takes all standard inputs as well as a given indicator variable "power" or "diff".  If there is no input for sample size, ind.sample will be executed
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param power A number.
#'@param n A number.
#'@param ind A quoted character string ("power" OR "diff").
#'@returns A data frame and plot of modeled sample size.
#'@examples
#'analyze.n(sdA0= 2, sdA1= 2, sdB0= 2, sdB1= 2, r1= c(.6,.7,.8), alpha= .05, m1= 54, m2= 55, power= c(.6,.7,.8), ind = "power")
#'@export
analyze.n  <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  if (missing(n)){
    ind.sample(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind)
  }
}

#'A function which will execute ind.diff function if the function is missing the mean difference variable.
#'
#'This function takes all standard inputs as well as a given indicator variable "power" or "sample".  If there is no input for m1 AND m2, ind.diff will be executed
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param power A number.
#'@param n A number.
#'@param ind A quoted character string ("power" OR "sample").
#'@returns A data frame and plot of modeled mean difference.
#'@examples
#'analyze.d(sdA0= 2, sdA1= 2, sdB0= 2, sdB1= 2, r1= c(.6,.7,.8), alpha= .05, power= c(.6,.7,.8), n= 64, ind= "power")
#'@export
analyze.d  <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  if (missing(m2) & missing(m1)){
    ind.diff(sdA0,sdA1,sdB0,sdB1,r1,alpha,power= power,n= n,ind = ind)
  }
}

#'A function which will execute pwr.sample function if the function is missing the power variable.
#'
#'This function takes all standard inputs as well as a given indicator variable "sample" or "diff".  If there is no input for power, ind.power will be executed
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param power A number.
#'@param n A number.
#'@param ind A quoted character string ("sample" OR "diff").
#'@returns A data frame and plot of modeled power.
#'@examples
#'analyze.p(sdA0= 2, sdA1= 2, sdB0= 2, sdB1= 2, r1= c(.6,.7,.8), alpha= .05, m1= .54, m2= 55, n= c(60,64,68), ind = "sample")
#'@export
analyze.p  <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  if (missing(power)){
    ind.power(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind)
  }
}
