
#'A function which executes one of the plot.sampfind functions based on a given indicator variable.
#'
#'This function takes standard inputs as well as a given indicator variable "power" or "diff" in order to distinguish and execute the correct function.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param power A number.
#'@param ind A quoted character string ("power" OR "diff").
#'@returns A data frame and plot of modeled sample size.
#'@examples
#'ind.sample(sdA0= 2, sdA1= 2, sdB0= 2, sdB1= 2, r1= c(.6,.7,.8), alpha= .05, m1= .54, m2= 55, power=  c(.6,.7,.8), ind = "power")
#'@export
ind.sample <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  if(ind == "diff"){
    s <- plot.sampfind.d(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power)
  }else{
    s <- plot.sampfind.p(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power)
  }
  return(s)
}

#'A function which executes one of the plot.diffind functions based on a given indicator variable.
#'
#'This function takes standard inputs as well as a given indicator variable "power" or "sample" in order to distinguish and execute the correct function.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param power A number.
#'@param n A number.
#'@param ind A quoted character string ("power" OR "sample").
#'@returns A data frame and plot of modeled mean difference.
#'@examples
#'ind.diff(sdA0= 2, sdA1= 2, sdB0= 2, sdB1= 2, r1= c(.6,.7,.8), alpha= .05, power= c(.6,.7,.8), n= 64,ind = "power")
#'@export
ind.diff <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  if(ind == "power"){
    s  <- plot.diffind.p(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,n = n)
  }else{
    s  <- plot.diffind.n(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,n)
  }
  return(s)
}

#'A function which executes one of the plot.pwrfind functions based on a given indicator variable.
#'
#'This function takes standard inputs as well as a given indicator variable "power" or "diff" in order to distinguish and execute the correct function.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param n A number.
#'@param ind A quoted character string ("diff" OR "sample").
#'@returns A data frame and plot of modeled mean difference.
#'@examples
#'ind.power(sdA0= 2, sdA1= 2, sdB0= 2, sdB1= 2, r1= c(.6,.7,.8), alpha= .05, m1= c(52,53,54), m2= c(54,55,56), n= 64,ind = "diff")
#'@export
ind.power <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  if(ind == "diff"){
    s <- plot.pwrfind.d(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,n)
  }else{
    s <- plot.pwrfind.n(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,n)
  }
  return(s)
}
