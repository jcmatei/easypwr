
#' Calculate and Plot Power-Related Variables.
#'
#' Takes the above inputs with the exeption of (m1 & m1), power, or n, and uses them to calculate the omitted variable and plot it against a specified indicator variable (ind) either "diff", "power", or "sample".
#' @param sdA0 A number.
#' @param sdA1 A number.
#' @param sdB0 A number.
#' @param sdB1 A number.
#' @param r1 A number.
#' @param alpha A number.
#' @param m1 A number.
#' @param m2 A number.
#' @param power A number.
#' @param n A number.
#' @param  A number.
#' @return A data frame and plot of inputted model.
#' @examples
#' analyze.inputs(sdA0 = 2,sdA1 = 2,sdB0 2= 2,sdB1 = 2,r1 = .5,alpha = .05,m1 = 54,m2 = 55,power = c(.6,.7,.8),          ind = "power")
#' @export
analyze.inputs <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind){

  p1 <-  analyze.p(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind)
  d <-   analyze.d(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind)
  n <- analyze.n(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power,n,ind)
  obj <- list(n,p1,d)
  obj <- clean.1(obj)
  obj <- clean.2(obj)
  return(obj)
}

