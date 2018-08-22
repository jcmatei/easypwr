#' Pool the Standard Deviation.
#'
#' @param sd0 A number.
#' @param sd1 A number.
#' @param r1 A number.
#' @return The pooled standard deviation of \code{sd0} and \code{sd1}.
#' @examples
#' psd(2, 2, .5)
#' @export

psd <- function(sd0, sd1, r1){

  pooled_sd <- sqrt((sd0)^2 + (sd1)^2 - (2*r1*sd0*sd1))
  return(pooled_sd)
}



#' Find the sample size based on given inputs.
#'
#' @param sdA0 A number.
#' @param sdA1 A number.
#' @param sdB0 A number.
#' @param sdB1 A number.
#' @param r1 A number.
#' @param alpha A number.
#' @param m1 A number.
#' @param m2 A number.
#' @param power A number
#' @return sammple size of the model
#' @examples
#' sample.find(2,2,2,2,.5,.05,55,54,.8)
#' @export

sample.find <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power){

  psa <- psd(sdA0, sdA1, r1)
  psb <- psd(sdB0, sdB1, r1)
  Za <- qnorm(1-(alpha/2))
  Zb <- qnorm(power)
  tau <- (psb^2)/(psa^2)
  gamma <- 1
  Nleft  <- ((Za+Zb)^2)*(tau+gamma)*(psa^2)/(gamma*(abs(m1-m2))^2)
  Nright1 <- ((gamma^3)+(tau^2))*(Za^2)
  Nright2 <-  2*gamma*((tau+gamma)^2)
  Nright <- Nright1/Nright2
  N <- Nleft+Nright
  return(N)
}

#' Find the difference of means based on given inputs.
#'
#' @param sdA0 A number.
#' @param sdA1 A number.
#' @param sdB0 A number.
#' @param sdB1 A number.
#' @param r1 A number.
#' @param alpha A number.
#' @param power A number.
#' @param n A number.
#' @return sammple size of the model
#' @examples
#' diff.find(2,2,2,2,.5,.05,.8,64)
#'@export

diff.find <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,N){

  psa <- psd(sdA0, sdA1, r1)
  psb <- psd(sdB0, sdB1, r1)
  Za <- qnorm(1-(alpha/2))
  Zb <- qnorm(power)
  tau <- (psb^2)/(psa^2)
  gamma <- 1
  Nlnum  <- ((Za+Zb)^2)*(tau+gamma)*(psa^2)
  Nright1 <- ((gamma^3)+(tau^2))*(Za^2)
  Nright2 <-  2*gamma*((tau+gamma)^2)
  Nright <- Nright1/Nright2
  es <-(Nlnum/(N-Nright))^.5
  return(es)
}

#' Find the power based on given inputs.
#'
#' @param sdA0 A number.
#' @param sdA1 A number.
#' @param sdB0 A number.
#' @param sdB1 A number.
#' @param r1 A number.
#' @param alpha A number.
#' @param m1 A number.
#' @param m2 A number.
#' @param n A number.
#' @return sammple size of the model
#' @examples
#' power.find(2,2,2,2,.5,.05,54,55,64)
#'@export

power.find<- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,n){
  psa <- psd(sdA0, sdA1, r1)
  psb <- psd(sdB0, sdB1, r1)
  Za <- qnorm(1-(alpha/2))
  tau <- (psb^2)/(psa^2)
  gamma <- 1
  Nlnuml <- (tau+gamma)*(psa^2)
  Nlden <- ((abs(m1-m2))^2)
  Nright1 <- ((gamma^3)+(tau^2))*(Za^2)
  Nright2 <-  2*gamma*((tau+gamma)^2)
  Nright <- Nright1/Nright2
  power_a <- pnorm(((Nlden*(n-Nright)/(Nlnuml))^(.5))-Za)
  return(power_a)
}


