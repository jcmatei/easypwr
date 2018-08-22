#'Cleans the list which is the output of analyze.input
#'
#'This function takes a three element list with two NULL elements and will either eliminate the first element if it is NULL or eliminate the last two if it is not.
#'@param obj A list.
#'@return A list of either length 1 or 2.
#'@examples
#'clean.1(obj)
#'@export
clean.1 <- function(obj){
  if(is.null(obj[[1]]) == TRUE){
    obj <- obj[-1]
  } else {obj <- obj[-2:-3]
  }
  return(obj)
}
#'Further cleans the list which is the output of analyze.input
#'
#'This function takes a two element list with one NULL element and will either eliminate the first element if it is NULL or eliminate the last one if it is not.
#'@param obj A list.
#'@return A list oflength 1.
#'@examples
#'clean.2(obj)
#'@export
clean.2 <- function(obj){
  if(is.null(obj[[1]]) == TRUE){
    obj <- obj[-1]
  } else {obj <- obj[-2]
  }
  return(obj)
}
