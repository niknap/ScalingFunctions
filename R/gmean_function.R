#' Geometric mean
#'
#' Calculation of geometric mean.
#' @param x Input vector
#' @param na.rm Boolean to remove missing values
#' @return Geometric mean
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

gmean <- function(x, na.rm=T, zero.rm=F){
  if(na.rm == T){
    x <- x[!is.na(x)]
  }
  if(zero.rm == T){
    x <- x[x != 0]
  }
  n <- length(x)
  return(prod(x^(1/n)))
}
