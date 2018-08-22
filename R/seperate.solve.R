#'Seperates data frame row and finds sample based on power
#'
#'This function is meant to allow data frames to be solved by row with apply.  It transforms a row into a matrix and uses sample.find across the columns of this one row matrix.
#'@param dfl A data frame row.
#'@return a vector of solved sample sizes.
#'@examples
#' sep_solve_np(df[1,])
#'@export
sep_solve_np <- function(dfl){

  mdfl <- matrix(dfl, byrow = FALSE)
  n <- sample.find(mdfl[1],mdfl[2],mdfl[3],mdfl[4],mdfl[5],mdfl[6],mdfl[7],mdfl[8],mdfl[9])
  return(n)

}

#'Seperates data frame row and finds sample based on mean difference
#'
#'This function is meant to allow data frames to be solved by row with apply.  It transforms a row into a matrix and uses sample.find across the columns of this one row matrix.
#'@param dfl A data frame row.
#'@return a vector of solved sample sizes.
#'@examples
#' sep_solve_nd(df[1,])
#'@export
sep_solve_nd <- function(dfl){

  mdfl <- matrix(dfl, byrow = FALSE)
  n <- sample.find(mdfl[1],mdfl[2],mdfl[3],mdfl[4],mdfl[5],mdfl[6],mdfl[8],mdfl[9],mdfl[7])
  return(n)

}

#'Seperates data frame row and finds sample power based on sample
#'
#'This function is meant to allow data frames to be solved by row with apply.  It transforms a row into a matrix and uses sample.find across the columns of this one row matrix.
#'@param dfl A data frame row.
#'@return a vector of solved powers.
#'@examples
#' sep_solve_pn(df[1,])
#'@export
sep_solve_pn <- function(dfl){

  mdfl <- matrix(dfl,byrow = FALSE)
  p <- power.find(mdfl[1],mdfl[2],mdfl[3],mdfl[4],mdfl[5],mdfl[6],mdfl[7],mdfl[8],mdfl[9])
  return(p)

}

#'Seperates data frame row and finds  power based on mean difference
#'
#'This function is meant to allow data frames to be solved by row with apply.  It transforms a row into a matrix and uses sample.find across the columns of this one row matrix.
#'@param dfl A data frame row.
#'@return a vector of solved powers.
#'@examples
#' sep_solve_pd(df[1,])
#'@export
sep_solve_pd <- function(dfl){

  mdfl <- matrix(dfl,byrow = FALSE)
  p <- power.find(mdfl[1],mdfl[2],mdfl[3],mdfl[4],mdfl[5],mdfl[6],mdfl[8],mdfl[9],mdfl[7])
  return(p)

}

#'Seperates data frame row and finds mean difference based on sample
#'
#'This function is meant to allow data frames to be solved by row with apply.  It transforms a row into a matrix and uses sample.find across the columns of this one row matrix.
#'@param dfl A data frame row.
#'@return a vector of solved mean differences.
#'@examples
#' sep_solve_dn(df[1,])
#'@export
sep_solve_dn <- function(dfl){

  mdfl <- matrix(dfl,byrow = FALSE)
  d <- diff.find(mdfl[1],mdfl[2],mdfl[3],mdfl[4],mdfl[5],mdfl[6],mdfl[7],mdfl[8])
  return(d)

}

#'Seperates data frame row and finds  mean difference based on power
#'
#'This function is meant to allow data frames to be solved by row with apply.  It transforms a row into a matrix and uses sample.find across the columns of this one row matrix.
#'@param dfl A data frame row.
#'@return a vector of solved mean differences.
#'@examples
#' sep_solve_dp(df[1,])
#'@export
sep_solve_dp <- function(dfl){


  mdfl <- matrix(dfl,byrow = FALSE)
  d <- diff.find(mdfl[1],mdfl[2],mdfl[3],mdfl[4],mdfl[5],mdfl[6],mdfl[8],mdfl[7])
  return(d)

}
